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

    -- Inserts a key into the map, overwriting existing values. Used for the start state of a let statement
    insert key value store = Map.insert key value store

     -- Set store1 to have the same value for the key as store2. Used for the return state of a let statement
    revert key store1 store2 =
        case Map.lookup key store2 of
            Just val -> Map.update (\x -> Just val) key store1
            Nothing -> Map.delete key store1