-- This module defines imp in bigstep semantics
module Imp_Bigstep (execute, Configuration (ConfPgm)) where
    import Imp_Syntax
    import Imp_State
    import qualified Data.Map.Strict as Map

    type State = (Map.Map String Integer)
    data Configuration =
        ConfAExp        AExp State |
        ConfIntConst    Integer    |
        ConfBExp        BExp State |
        ConfBoolConst   Bool       |
        ConfStatement   Stmt State |
        ConfState       State      |
        ConfPgm         Pgm        |
        ConfError                  | -- New configurations for error and halting
        ConfHalting         State
        deriving Show

    -- There's no non determinism handling because it wouldn't matter: the language has no side effects.
    -- All the rules are updated to check for errors or the halting state where necessary
    execute :: Configuration -> Configuration
    execute conf =
        case conf of
            ConfPgm pgm -> 
                    (case pgm of
                        Init vars stmt -> case execute (ConfStatement stmt (Imp_State.from_list vars)) of
                                            ConfState state -> ConfState state
                                            ConfHalting state -> ConfState state) -- Hide halting state
            ConfStatement stmt state -> 
                    (case stmt of
                        Assignment  var aexp -> (case execute (ConfAExp aexp state) of
                                                    ConfIntConst i -> ConfState (Imp_State.update var i state)
                                                    ConfError -> ConfHalting state)
                        SeqComp     stmt1 stmt2  -> (case execute (ConfStatement stmt1 state) of
                                                        ConfState state2 -> (case execute (ConfStatement stmt2 state2) of
                                                                                ConfState state3 -> ConfState state3
                                                                                ConfHalting state3 -> ConfHalting state3)
                                                        ConfHalting state2 -> ConfHalting state2)
                        If          bexp stmt1 stmt2 -> (case execute (ConfBExp bexp state) of
                                                            ConfBoolConst True -> (case execute (ConfStatement stmt1 state) of
                                                                                                    ConfState state2 -> ConfState state2
                                                                                                    ConfHalting state2 -> ConfHalting state2)
                                                            ConfBoolConst False -> (case execute (ConfStatement stmt2 state) of
                                                                                                    ConfState state2 -> ConfState state2
                                                                                                    ConfHalting state2 -> ConfHalting state2)
                                                            ConfError -> ConfHalting state)
                        While       bexp stmt  -> (case execute (ConfBExp bexp state) of
                                                            ConfBoolConst True -> (case execute (ConfStatement (SeqComp stmt (While bexp stmt)) state) of
                                                                                        ConfState state2 -> ConfState state2
                                                                                        ConfHalting state2 -> ConfHalting state2)
                                                            ConfBoolConst False -> ConfState state
                                                            ConfError -> ConfHalting state)
                        Block       stmt -> (case execute (ConfStatement stmt state) of
                                                    ConfState state2 -> ConfState state2
                                                    ConfHalting state2 -> ConfHalting state2)
                        Skip -> ConfState state
                        Halt -> ConfHalting state) --NEW
            ConfBExp bexp state ->
                    (case bexp of
                        BConst      bool -> ConfBoolConst bool
                        BAndExp     bexp1 bexp2 -> (case execute (ConfBExp bexp1 state) of
                                                            ConfBoolConst True -> (case execute (ConfBExp bexp2 state) of
                                                                                        ConfBoolConst t -> ConfBoolConst t
                                                                                        ConfError -> ConfError)
                                                            ConfBoolConst False -> ConfBoolConst False
                                                            ConfError -> ConfError)
                        BLtEqExp    aexp1 aexp2 ->  let a1 = execute (ConfAExp aexp1 state) in
                                                    case a1 of
                                                        ConfIntConst i1 -> let a2 = execute (ConfAExp aexp2 state) in
                                                            (case a2 of
                                                                ConfIntConst i2 -> ConfBoolConst (i1 <= i2)
                                                                ConfError -> ConfError)
                                                        ConfError -> ConfError
                        BNotExp     bexp -> (case execute (ConfBExp bexp state) of
                                                            ConfBoolConst True -> ConfBoolConst False
                                                            ConfBoolConst False -> ConfBoolConst True
                                                            ConfError -> ConfError))
            ConfAExp aexp state ->
                    (case aexp of
                        Id      id -> ConfIntConst (Imp_State.get id state)
                        AConst  i -> ConfIntConst i
                        ABinExp op aexp1 aexp2 ->   let a1 = execute (ConfAExp aexp1 state) in
                                                    case a1 of
                                                        ConfIntConst i1 -> let a2 = execute (ConfAExp aexp2 state) in
                                                            (case a2 of
                                                                ConfIntConst i2 -> (case op of 
                                                                                        Plus -> ConfIntConst (i1 + i2)
                                                                                        Divide -> case i2 /= 0 of
                                                                                            True -> ConfIntConst (div i1 i2)
                                                                                            False -> ConfError)
                                                                ConfError -> ConfError)
                                                        ConfError -> ConfError)


