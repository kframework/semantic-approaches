-- This module defines imp in bigstep semantics along with increment
module Imp_Bigstep (execute, Configuration (ConfPgm)) where
    import Imp_Syntax
    import Imp_State
    import qualified Data.Map.Strict as Map
    import qualified Data.List as List

    type State = (Map.Map String Integer)
    data Configuration =
        ConfAExp        AExp State |
        ConfBExp        BExp State |
        -- ConfIntConst    Integer | These are no longer needed, and all the rules that used them were updated.
        -- ConfBoolConst   Bool    |
        ConfStatement   Stmt State |
        ConfState       State      |
        ConfPgm         Pgm
        deriving (Show, Eq)

    -- Non determinism is modeled through a list of configurations. Instead of returning one configuration, execute returns one for every possible execution.
    -- This means that every rule needs to be modified from a case statement on the result to apply that statement to every result.
    -- Also note that concat is equivalent to flatten - whenever there are multiple execute calls they need to be flattened before returning
    execute :: Configuration -> [Configuration]
    execute conf =
        case conf of
            ConfPgm pgm -> 
                    (case pgm of
                        Init vars stmt ->
                                            let onEvalPgm pgm = case pgm of ConfState state -> ConfState state in
                                            map onEvalPgm (execute (ConfStatement stmt (Imp_State.from_list vars))))
            ConfStatement stmt state -> 
                    (case stmt of
                        Assignment  id aexp ->
                                                let onEvalAexp aexp = case aexp of ConfAExp (AConst i) state1 -> ConfState (Imp_State.update id i state1) in
                                                map onEvalAexp (execute (ConfAExp aexp state))
                        SeqComp     stmt1 stmt2 ->
                                                let onEvalStmt2 stmt2 = case stmt2 of ConfState state3 -> ConfState state3 in
                                                let onEvalStmt1 stmt1 = case stmt1 of ConfState state2 -> map onEvalStmt2 (execute (ConfStatement stmt2 state2)) in
                                                concat (map onEvalStmt1 (execute (ConfStatement stmt1 state)))
                        If          bexp stmt1 stmt2 ->
                                                let onEvalStmt stmt = case stmt of ConfState state2 -> ConfState state2 in
                                                let onEvalBexp bexpE = (case bexpE of
                                                                        ConfBExp (BConst True) state1 -> map onEvalStmt (execute (ConfStatement stmt1 state1))
                                                                        ConfBExp (BConst False) state1 -> map onEvalStmt (execute (ConfStatement stmt2 state1))) in
                                                concat (map onEvalBexp (execute (ConfBExp bexp state)))
                        While       bexp stmt  ->
                                                let onEvalNewWhile stmt2 = case stmt2 of ConfState state2 -> ConfState state2 in
                                                let onEvalBexp bexpE = (case bexpE of
                                                                        ConfBExp (BConst True) state1 -> map onEvalNewWhile (execute (ConfStatement (SeqComp stmt (While bexp stmt)) state1))
                                                                        ConfBExp (BConst False) state1 -> [ConfState state1]) in
                                                concat (map onEvalBexp (execute (ConfBExp bexp state)))
                        Block       stmt ->
                                                let onEvalStmt stmt = case stmt of ConfState state2 -> ConfState state2 in
                                                map onEvalStmt (execute (ConfStatement stmt state))
                        Skip -> [ConfState state])
            ConfBExp bexp state ->
                    (case bexp of
                        BConst      bool -> [ConfBExp (BConst bool) state]
                        BAndExp     bexp1 bexp2 ->
                                                    let onEvalBexp2 bexp2 = case bexp2 of ConfBExp (BConst t) state2  -> ConfBExp (BConst t) state2 in
                                                    let onEvalBexp1 bexp1 = (case bexp1 of
                                                                                ConfBExp (BConst True) state1 -> map onEvalBexp2 (execute (ConfBExp bexp2 state1))
                                                                                ConfBExp (BConst False) state1 -> [ConfBExp (BConst False) state1]) in
                                                    concat (map onEvalBexp1 (execute (ConfBExp bexp1 state)))
                        BLtEqExp    aexp1 aexp2 ->  
                                                    let onEvalAexp1 aexp1 = (case aexp1 of
                                                                                ConfAExp (AConst i1) state1 -> 
                                                                                    let onEvalAexp2 aexp2 = case aexp2 of ConfAExp (AConst i2) state2 -> ConfBExp (BConst (i1 <= i2)) state2 in
                                                                                    map onEvalAexp2 (execute (ConfAExp aexp2 state1))) in
                                                    concat (map onEvalAexp1 (execute (ConfAExp aexp1 state)))
                        BNotExp     bexp -> 
                                                    let onEvalBexp bexp = (case bexp of
                                                                            ConfBExp (BConst True) state1 -> ConfBExp (BConst False) state1
                                                                            ConfBExp (BConst False) state1 -> ConfBExp (BConst True) state1) in
                                                    map onEvalBexp (execute (ConfBExp bexp state)))
            ConfAExp aexp state ->
                    (case aexp of
                        Id      id -> [ConfAExp (AConst (Imp_State.get id state)) state]
                        PlusPlus (Id id) -> [ConfAExp (AConst ((Imp_State.get id state) + 1)) (Imp_State.update id ((Imp_State.get id state) + 1) state)] -- NEW
                        AConst  i -> [ConfAExp (AConst i) state]
                         -- This is the only case that returns two states, one from evaluating the first argument first and the other from evaluating the second argument first. Every rule had to be updated to handle this.
                        ABinExp op aexp1 aexp2 ->   let evalExps aexp1O aexp2O okfunc dfunc = -- okfunc is used to tell if the divisor is not 0 and dfunc does the division, these are added to reuse code
                                                            let onEvalAexp1 aexp1E = (case aexp1E of
                                                                                    ConfAExp (AConst i1) state1 ->
                                                                                        let onEvalExp2 aexp2E = (case aexp2E of ConfAExp (AConst i2) state2 -> (case op of 
                                                                                                                                                                Plus -> ConfAExp (AConst (i1 + i2)) state2
                                                                                                                                                                Divide -> case (okfunc i1 i2) of True -> ConfAExp (AConst (dfunc i1 i2)) state2)) in
                                                                                        map onEvalExp2 (execute (ConfAExp aexp2O state1))) in
                                                            concat (map onEvalAexp1 (execute (ConfAExp aexp1O state))) in
                                                    -- Remove equivalent states generated by multiplication or division that didn't matter (i.e. 8 / 2 is always 4 regardless of which one is evaluated first, but we don't know that at execution)
                                                    -- Do this here rather than once at the end b/c otherwise the number of configurations explodes needlessly and computation never finishes
                                                    List.nub (concat [(evalExps aexp1 aexp2 (\i1 i2 -> i2 /=0) (\i1 i2 -> div i1 i2)),
                                                                        (evalExps aexp2 aexp1 (\i1 i2 -> i1 /=0) (\i1 i2 -> div i2 i1))]))
