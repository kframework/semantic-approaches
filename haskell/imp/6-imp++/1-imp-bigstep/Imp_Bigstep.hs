-- This module defines imp++ in bigstep semantics along with increment
module Imp_Bigstep (execute, Configuration (ConfPgm)) where
    import Imp_Syntax
    import Imp_State
    import Imp_Buffer
    import qualified Data.Map.Strict as Map
    import qualified Data.List as List

    -- This module combines all the changes from 1-5. Explanations of the additions can be found in their respective parts, but new parts (except for error handling) are marked with NEW.

    type State = (Map.Map String Integer)
    type Buffer = [Integer]
    data Configuration =
        ConfAExp        AExp    State   Buffer |
        ConfBExp        BExp    State   Buffer |
        ConfStatement   Stmt    State   Buffer |
        ConfState       State   Buffer  Buffer |
        ConfHalting     State   Buffer  Buffer |
        ConfError       State   Buffer         |
        ConfPgm         Pgm Buffer
        deriving (Show, Eq)

    -- Non determinism is modeled through a list of configurations. Instead of returning one configuration, execute returns one for every possible execution. 
    execute :: Configuration -> [Configuration]
    execute conf =
        case conf of
            ConfPgm pgm input -> 
                    (case pgm of
                        Init vars stmt ->
                                            let onEvalPgm pgm = case pgm of
                                                                    ConfState state input output -> ConfState state input output
                                                                    ConfHalting state input output -> ConfState state input output in
                                            List.nub (map onEvalPgm (execute (ConfStatement stmt (Imp_State.from_list vars) input))))
            ConfStatement stmt state input -> 
                    (case stmt of
                        Assignment  id aexp ->
                                                let onEvalAexp aexp = case aexp of
                                                                        ConfAExp (AConst i) state1 input1 -> ConfState (Imp_State.update id i state1) input1 []
                                                                        ConfError state1 input1 -> ConfHalting state1 input1 [] in
                                                map onEvalAexp (execute (ConfAExp aexp state input))
                        SeqComp     stmt1 stmt2 ->
                                                let compose stmt1 stmt2 =
                                                        let onEvalStmt1 stmt1 = case stmt1 of
                                                                ConfState state2 input1 output1 -> 
                                                                    let onEvalStmt2 stmt2 = case stmt2 of
                                                                                            ConfState state3 input2 output2 -> ConfState state3 input2 (Imp_Buffer.combine output1 output2)
                                                                                            ConfHalting state3 input2 output2 -> ConfHalting state3 input2 (Imp_Buffer.combine output1 output2) in
                                                                        map onEvalStmt2 (execute (ConfStatement stmt2 state2 input1))
                                                                ConfHalting state2 input1 output1 -> [ConfHalting state2 input1 output1] in
                                                        concat (map onEvalStmt1 (execute (ConfStatement stmt1 state input))) in
                                                (case stmt1 of  --NEW
                                                    Spawn block -> List.nub (concat [(compose block stmt2), (compose stmt2 block)])
                                                    _ -> compose stmt1 stmt2)
                        If          bexp stmt1 stmt2 ->
                                                let onEvalStmt stmt = case stmt of
                                                                        ConfState state2 input2 output2 -> ConfState state2 input2 output2
                                                                        ConfHalting state2 input2 output2 -> ConfHalting state2 input2 output2 in
                                                let onEvalBexp bexpE = (case bexpE of
                                                                        ConfBExp (BConst True) state1 input1 -> map onEvalStmt (execute (ConfStatement stmt1 state1 input1))
                                                                        ConfBExp (BConst False) state1 input1 -> map onEvalStmt (execute (ConfStatement stmt2 state1 input1))
                                                                        ConfError state1 input1 -> [ConfHalting state1 input1 []]) in
                                                concat (map onEvalBexp (execute (ConfBExp bexp state input)))
                        While       bexp stmt  ->
                                                let onEvalNewWhile stmt2 = case stmt2 of
                                                                            ConfState state2 input2 output2 -> ConfState state2 input2 output2 
                                                                            ConfHalting state2 input2 output2 -> ConfHalting state2 input2 output2 in
                                                let onEvalBexp bexpE = (case bexpE of
                                                                        ConfBExp (BConst True) state1 input1 -> map onEvalNewWhile (execute (ConfStatement (SeqComp stmt (While bexp stmt)) state1 input1))
                                                                        ConfBExp (BConst False) state1 input1 -> [ConfState state1 input1 []]
                                                                        ConfError state1 input1 -> [ConfHalting state1 input1 []]) in
                                                concat (map onEvalBexp (execute (ConfBExp bexp state input)))
                        Block       stmt ->
                                                let onEvalStmt stmt = case stmt of
                                                                        ConfState state2 input1 output1 -> ConfState state2 input1 output1
                                                                        ConfHalting state2 input1 output1 -> ConfHalting state2 input1 output1 in
                                                map onEvalStmt (execute (ConfStatement stmt state input))
                        Print       aexp -> --NEW
                                                let onEvalAexp aexp = case aexp of
                                                                        ConfAExp (AConst i) state1 input1 -> ConfState state1 input1 [i]
                                                                        ConfError state1 input1 -> ConfHalting state1 input1 [] in
                                                map onEvalAexp (execute (ConfAExp aexp state input))
                        Spawn       block -> -- NEW
                                                let onEvalBlock block = case block of ConfState state2 input2 output2-> ConfState state2 input2 output2 in
                                                map onEvalBlock (execute (ConfStatement block state input))
                        Let     var aexp stmt -> --NEW
                                                let onEvalAexp aexp = case aexp of
                                                                        ConfAExp (AConst i) state1 input1 -> 
                                                                            let onEvalStmt stmt = 
                                                                                    case stmt of 
                                                                                        ConfHalting state2 input2 ouput2 -> ConfHalting state2 input2 ouput2 
                                                                                        ConfState state2 input2 output2 -> ConfState (Imp_State.revert var state2 state1) input2 output2 in
                                                                            map onEvalStmt (execute (ConfStatement stmt (Imp_State.insert var i state1) input1))
                                                                        ConfError state1 input1 -> [ConfHalting state1 input1 []] in
                                                concat (map onEvalAexp (execute (ConfAExp aexp state input)))
                        Skip -> [ConfState state input []]  --NEW
                        Halt -> [ConfHalting state input []])  --NEW
            ConfBExp bexp state input ->
                    (case bexp of
                        BConst      bool -> [ConfBExp (BConst bool) state input]
                        BAndExp     bexp1 bexp2 ->
                                                    let onEvalBexp2 bexp2 = case bexp2 of 
                                                                                ConfBExp (BConst t) state2 input2  -> ConfBExp (BConst t) state2 input2
                                                                                ConfError state2 input2 -> ConfError state2 input2 in
                                                    let onEvalBexp1 bexp1 = (case bexp1 of
                                                                                ConfBExp (BConst True) state1 input1 -> map onEvalBexp2 (execute (ConfBExp bexp2 state1 input1))
                                                                                ConfBExp (BConst False) state1 input1 -> [ConfBExp (BConst False) state1 input1]
                                                                                ConfError state1 input1 -> [ConfError state1 input1]) in
                                                    concat (map onEvalBexp1 (execute (ConfBExp bexp1 state input)))
                        BLtEqExp    aexp1 aexp2 ->  
                                                    let onEvalAexp1 aexp1 = (case aexp1 of
                                                                                ConfAExp (AConst i1) state1 input1 -> 
                                                                                    let onEvalAexp2 aexp2 = case aexp2 of
                                                                                                                ConfAExp (AConst i2) state2 input2 -> ConfBExp (BConst (i1 <= i2)) state2 input2
                                                                                                                ConfError state2 input2 -> ConfError state2 input2 in
                                                                                    map onEvalAexp2 (execute (ConfAExp aexp2 state1 input1))
                                                                                ConfError state1 input1 -> [ConfError state1 input1]) in
                                                    concat (map onEvalAexp1 (execute (ConfAExp aexp1 state input)))
                        BNotExp     bexp -> 
                                                    let onEvalBexp bexp = (case bexp of
                                                                            ConfBExp (BConst True) state1 input1 -> ConfBExp (BConst False) state1 input1
                                                                            ConfBExp (BConst False) state1 input1 -> ConfBExp (BConst True) state1 input1
                                                                            ConfError state1 input1 -> ConfError state1 input1) in
                                                    map onEvalBexp (execute (ConfBExp bexp state input)))
            ConfAExp aexp state input ->
                    (case aexp of
                        Id      id -> [ConfAExp (AConst (Imp_State.get id state)) state input]
                        PlusPlus (Id id) -> [ConfAExp (AConst ((Imp_State.get id state) + 1)) (Imp_State.update id ((Imp_State.get id state) + 1) state) input] -- NEW
                        AConst  i -> [ConfAExp (AConst i) state input]
                        Read -> case Imp_Buffer.split input of (head, rest) -> [ConfAExp (AConst head) state rest]  --NEW
                        ABinExp op aexp1 aexp2 ->   let evalExps aexp1O aexp2O okfunc dfunc =
                                                            let onEvalAexp1 aexp1E = (case aexp1E of
                                                                                        ConfAExp (AConst i1) state1 input1 ->
                                                                                            (let onEvalExp2 aexp2E = (case aexp2E of
                                                                                                                            ConfAExp (AConst i2) state2 input2 -> (case op of 
                                                                                                                                                                        Plus -> ConfAExp (AConst (i1 + i2)) state2 input2
                                                                                                                                                                        Divide -> (case (okfunc i1 i2) of
                                                                                                                                                                                    True -> ConfAExp (AConst (dfunc i1 i2)) state2 input2
                                                                                                                                                                                    False -> ConfError state2 input2))
                                                                                                                            ConfError state2 input2 -> ConfError state2 input2) in
                                                                                            map onEvalExp2 (execute (ConfAExp aexp2O state1 input1)))
                                                                                        ConfError state1 input1 -> [ConfError state1 input1]) in
                                                            concat (map onEvalAexp1 (execute (ConfAExp aexp1O state input))) in

                                                    List.nub (concat [(evalExps aexp1 aexp2 (\i1 i2 -> i2 /=0) (\i1 i2 -> div i1 i2))]))
