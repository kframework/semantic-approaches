-- This module defines imp in bigstep semantics
module Imp_Bigstep (execute, Configuration (ConfPgm)) where
    import Imp_Syntax
    import Imp_State
    import Imp_Buffer
    import qualified Data.Map.Strict as Map
    import qualified Data.List as List

    -- NEW: [Integer] is a list of integers, acting as either an output or input buffer. All the rules are updated for the new buffers
    type State = (Map.Map String Integer)
    type Buffer = [Integer]
    data Configuration =
        ConfAExp        AExp State Buffer   |
        ConfIntConst    Integer Buffer      |
        ConfBExp        BExp State Buffer   |
        ConfBoolConst   Bool    Buffer      |
        ConfStatement   Stmt State Buffer   |
        ConfState       State Buffer Buffer |
        ConfPgm         Pgm Buffer
        deriving (Show, Eq)

    -- Non determinism is modeled through a list of configurations. Instead of returning one configuration, execute returns one for every possible execution. 
    execute :: Configuration -> [Configuration]
    execute conf =
        case conf of
            ConfPgm pgm input -> 
                    (case pgm of
                        Init vars stmt ->
                                                let onEvalPgm pgm = case pgm of ConfState state input output -> ConfState state input output in
                                                map onEvalPgm (execute (ConfStatement stmt (Imp_State.from_list vars) input)))
            ConfStatement stmt state input -> 
                    (case stmt of
                        Assignment  id aexp ->
                                                let onEvalAexp aexp = case aexp of ConfIntConst i input1 -> ConfState (Imp_State.update id i state) input1 [] in
                                                map onEvalAexp (execute (ConfAExp aexp state input))
                        SeqComp     stmt1 stmt2 ->
                                                let onEvalStmt1 stmt1 = case stmt1 of
                                                        ConfState state2 input1 output1 ->
                                                            let onEvalStmt2 stmt2 = case stmt2 of ConfState state3 input2 output2 -> ConfState state3 input2 (Imp_Buffer.combine output1 output2) in
                                                            map onEvalStmt2 (execute (ConfStatement stmt2 state2 input1)) in
                                                concat (map onEvalStmt1 (execute (ConfStatement stmt1 state input)))
                        If          bexp stmt1 stmt2 ->
                                                let onEvalStmt stmt = case stmt of ConfState state2 input2 output2-> ConfState state2 input2 output2 in
                                                let onEvalBexp bexpE = (case bexpE of
                                                                        ConfBoolConst True input1 -> map onEvalStmt (execute (ConfStatement stmt1 state input1))
                                                                        ConfBoolConst False input1 -> map onEvalStmt (execute (ConfStatement stmt2 state input1))) in
                                                concat (map onEvalBexp (execute (ConfBExp bexp state input)))
                        While       bexp stmt  ->
                                                let onEvalNewWhile stmt2 = case stmt2 of ConfState state2 input2 output2 -> ConfState state2 input2 output2 in
                                                let onEvalBexp bexpE = (case bexpE of
                                                                        ConfBoolConst True input1 -> map onEvalNewWhile (execute (ConfStatement (SeqComp stmt (While bexp stmt)) state input1))
                                                                        ConfBoolConst False input1 -> [ConfState state input1 []]) in
                                                concat (map onEvalBexp (execute (ConfBExp bexp state input)))
                        Print       aexp ->
                                            let onEvalAexp aexp = case aexp of ConfIntConst i input1 -> ConfState state input1 [i] in
                                            map onEvalAexp (execute (ConfAExp aexp state input)) -- NEW
                        Block       stmt ->
                                                let onEvalStmt stmt = case stmt of ConfState state2 input1 output1 -> ConfState state2 input1 output1 in
                                                map onEvalStmt (execute (ConfStatement stmt state input))
                        Skip -> [ConfState state input []])
            ConfBExp bexp state input ->
                    (case bexp of
                        BConst      bool -> [ConfBoolConst bool input]
                        BAndExp     bexp1 bexp2 ->
                                                    let onEvalBexp2 bexp2 = case bexp2 of ConfBoolConst t input2  -> ConfBoolConst t input2 in
                                                    let onEvalBexp1 bexp1 = (case bexp1 of
                                                                                ConfBoolConst True input1 -> map onEvalBexp2 (execute (ConfBExp bexp2 state input1))
                                                                                ConfBoolConst False input1 -> [ConfBoolConst False input1]) in
                                                    concat (map onEvalBexp1 (execute (ConfBExp bexp1 state input)))
                        BLtEqExp    aexp1 aexp2 ->  
                                                    let onEvalAexp1 aexp1 = (case aexp1 of
                                                                                ConfIntConst i1 input1 -> 
                                                                                    let onEvalAexp2 aexp2 = case aexp2 of ConfIntConst i2 input2 -> ConfBoolConst (i1 <= i2) input2 in
                                                                                    map onEvalAexp2 (execute (ConfAExp aexp2 state input1))) in
                                                    concat (map onEvalAexp1 (execute (ConfAExp aexp1 state input)))
                        BNotExp     bexp -> 
                                                    let onEvalBexp bexp = (case bexp of
                                                                            ConfBoolConst True input1 -> ConfBoolConst False input1
                                                                            ConfBoolConst False input1 -> ConfBoolConst True input1) in
                                                    map onEvalBexp (execute (ConfBExp bexp state input)))
            ConfAExp aexp state input ->
                    (case aexp of
                        Id      id -> [ConfIntConst (Imp_State.get id state) input]
                        AConst  i -> [ConfIntConst i input]
                        Read -> case Imp_Buffer.split input of (head, rest) -> [ConfIntConst head rest] -- NEW
                         -- This is the only case that returns two states, one from evaluating the first argument first and the other from evaluating the second argument first. Every rule had to be updated to handle this.
                        ABinExp op aexp1 aexp2 ->   let evalExps aexp1O aexp2O okfunc dfunc = -- okfunc is used to tell if the divisor is not 0 and dfunc does the division, these are added to reuse code
                                                            let onEvalAexp1 aexp1E = (case aexp1E of
                                                                                    ConfIntConst i1 input1 ->
                                                                                        let onEvalExp2 aexp2E = (case aexp2E of ConfIntConst i2 input2 -> (case op of 
                                                                                                                                                                Plus -> ConfIntConst (i1 + i2) input2
                                                                                                                                                                Divide -> (case (okfunc i1 i2) of True -> ConfIntConst (dfunc i1 i2) input2))) in
                                                                                        map onEvalExp2 (execute (ConfAExp aexp2O state input1))) in
                                                            concat (map onEvalAexp1 (execute (ConfAExp aexp1O state input))) in
                                                    -- Remove equivalent states generated by multiplication or division that didn't matter (i.e. 8 / 2 is always 4 regardless of which one is evaluated first, but we don't know that at execution)
                                                    -- Do this here rather than once at the end b/c otherwise the number of configurations explodes needlessly and computation never finishes
                                                    List.nub (concat [(evalExps aexp1 aexp2 (\i1 i2 -> i2 /=0) (\i1 i2 -> div i1 i2)),
                                                                        (evalExps aexp2 aexp1 (\i1 i2 -> i1 /=0) (\i1 i2 -> div i2 i1))]))
