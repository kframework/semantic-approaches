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
        ConfIntConst    Integer    |
        ConfBoolConst   Bool       |
        ConfStatement   Stmt State |
        ConfState       State      |
        ConfPgm         Pgm
        deriving (Show, Eq)

    -- Non determinism is modeled through a list of configurations. Instead of returning one configuration, execute returns one for every possible execution. 
    execute :: Configuration -> [Configuration]
    execute conf =
        case conf of
            ConfPgm pgm -> 
                    (case pgm of
                        Init vars stmt ->
                                            let onEvalPgm pgm = case pgm of ConfState state -> ConfState state in
                                            List.nub (map onEvalPgm (execute (ConfStatement stmt (Imp_State.from_list vars))))) -- Remove any remaining duplicate states
            ConfStatement stmt state -> 
                    (case stmt of
                        Assignment  id aexp ->
                                                let onEvalAexp aexp = case aexp of ConfIntConst i -> ConfState (Imp_State.update id i state) in
                                                map onEvalAexp (execute (ConfAExp aexp state))
                        SeqComp     stmt1 stmt2 -> -- NEW. This follows the same desperate attempt approach as in maude and either runs the spawn first or the block after first
                                                let compose stmt1 stmt2 = 
                                                        let onEvalStmt2 stmt2 = case stmt2 of ConfState state3 -> ConfState state3 in
                                                        let onEvalStmt1 stmt1 = case stmt1 of ConfState state2 -> map onEvalStmt2 (execute (ConfStatement stmt2 state2)) in
                                                        concat (map onEvalStmt1 (execute (ConfStatement stmt1 state))) in
                                                case stmt1 of
                                                    -- Remove equivalent configurations early to prevent blowup
                                                    Spawn block -> List.nub (concat [(compose block stmt2), (compose stmt2 block)])
                                                    _ -> compose stmt1 stmt2
                        If          bexp stmt1 stmt2 ->
                                                let onEvalStmt stmt = case stmt of ConfState state2 -> ConfState state2 in
                                                let onEvalBexp bexpE = (case bexpE of
                                                                        ConfBoolConst True -> map onEvalStmt (execute (ConfStatement stmt1 state))
                                                                        ConfBoolConst False -> map onEvalStmt (execute (ConfStatement stmt2 state))) in
                                                concat (map onEvalBexp (execute (ConfBExp bexp state)))
                        While       bexp stmt  ->
                                                let onEvalNewWhile stmt2 = case stmt2 of ConfState state2 -> ConfState state2 in
                                                let onEvalBexp bexpE = (case bexpE of
                                                                        ConfBoolConst True -> map onEvalNewWhile (execute (ConfStatement (SeqComp stmt (While bexp stmt)) state))
                                                                        ConfBoolConst False -> [ConfState state]) in
                                                concat (map onEvalBexp (execute (ConfBExp bexp state)))
                        Block       stmt ->
                                                let onEvalStmt stmt = case stmt of ConfState state2 -> ConfState state2 in
                                                map onEvalStmt (execute (ConfStatement stmt state))
                        Skip -> [ConfState state]
                        Spawn       block -> -- NEW
                                                let onEvalBlock block = case block of ConfState state2 -> ConfState state2 in
                                                map onEvalBlock (execute (ConfStatement block state)))
            ConfBExp bexp state ->
                    (case bexp of
                        BConst      bool -> [ConfBoolConst bool]
                        BAndExp     bexp1 bexp2 ->
                                                    let onEvalBexp2 bexp2 = case bexp2 of ConfBoolConst t  -> ConfBoolConst t in
                                                    let onEvalBexp1 bexp1 = (case bexp1 of
                                                                                ConfBoolConst True -> map onEvalBexp2 (execute (ConfBExp bexp2 state))
                                                                                ConfBoolConst False -> [ConfBoolConst False]) in
                                                    concat (map onEvalBexp1 (execute (ConfBExp bexp1 state)))
                        BLtEqExp    aexp1 aexp2 ->  
                                                    let onEvalAexp1 aexp1 = (case aexp1 of
                                                                                ConfIntConst i1 -> 
                                                                                    let onEvalAexp2 aexp2 = case aexp2 of ConfIntConst i2 -> ConfBoolConst (i1 <= i2) in
                                                                                    map onEvalAexp2 (execute (ConfAExp aexp2 state))) in
                                                    concat (map onEvalAexp1 (execute (ConfAExp aexp1 state)))
                        BNotExp     bexp -> 
                                                    let onEvalBexp bexp = (case bexp of
                                                                            ConfBoolConst True -> ConfBoolConst False
                                                                            ConfBoolConst False -> ConfBoolConst True) in
                                                    map onEvalBexp (execute (ConfBExp bexp state)))
            ConfAExp aexp state ->
                    (case aexp of
                        Id      id -> [ConfIntConst (Imp_State.get id state)]
                        AConst  i -> [ConfIntConst i]
                        ABinExp op aexp1 aexp2 ->   
                                                    let onEvalAexp1 aexp1E = (case aexp1E of
                                                                            ConfIntConst i1 ->
                                                                                let onEvalExp2 aexp2E = (case aexp2E of ConfIntConst i2 -> (case op of 
                                                                                                                                                        Plus -> ConfIntConst (i1 + i2)
                                                                                                                                                        Divide -> case i2 /= 0 of True -> ConfIntConst (div i1 i2))) in
                                                                                map onEvalExp2 (execute (ConfAExp aexp2 state))) in
                                                    concat (map onEvalAexp1 (execute (ConfAExp aexp1 state))))
