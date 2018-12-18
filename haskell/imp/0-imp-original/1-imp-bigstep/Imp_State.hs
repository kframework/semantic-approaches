-- This module defines convenience functions for a key value store
module Imp_State where
    import qualified Data.Map.Strict as Map

    from_list list = Map.fromList (map (\i -> (i, 0)) list)

    get key store =
        case Map.lookup key store of
            Just val -> val
            Nothing -> error ((show key) ++ " is not in the store")

    update key value store = 
        case Map.lookup key store of
            Just _ -> Map.update (\x -> Just value) key store
            Nothing -> error ((show key) ++ " is not in the store")
