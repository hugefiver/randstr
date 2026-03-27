-- | Utility functions.
module RandStr.Utils
  ( removeDuplicatesPreservingOrder
  ) where

import qualified Data.Set as Set

-- | Remove duplicates from a list while preserving the order of first occurrences.
--
-- >>> removeDuplicatesPreservingOrder "aabbcc"
-- "abc"
-- >>> removeDuplicatesPreservingOrder [3,1,2,1,3]
-- [3,1,2]
removeDuplicatesPreservingOrder :: Ord a => [a] -> [a]
removeDuplicatesPreservingOrder = go Set.empty
  where
    go _ [] = []
    go seen (x:xs)
      | x `Set.member` seen = go seen xs
      | otherwise           = x : go (Set.insert x seen) xs
