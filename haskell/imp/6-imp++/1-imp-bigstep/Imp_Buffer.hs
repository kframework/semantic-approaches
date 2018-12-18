-- This module defines convenience functions for a buffer (just a list)
module Imp_Buffer where
    import qualified Data.List as List

    combine list1 list2 = list1 ++ list2

    split list = case List.uncons list of
                    Just (head, rest) -> (head, rest)
                    Nothing -> error "Can't split empty list"
