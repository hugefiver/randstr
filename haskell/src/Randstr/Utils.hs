-- | Utility functions for the randstr library.
module Randstr.Utils
  ( removeDuplicatesPreservingOrder
  ) where

import qualified Data.Set as Set

-- | Remove duplicates from a list while preserving the original order.
-- Uses a Set for O(log n) lookup.
removeDuplicatesPreservingOrder :: Ord a => [a] -> [a]
removeDuplicatesPreservingOrder = go Set.empty
  where
    go _ [] = []
    go seen (x:xs)
      | x `Set.member` seen = go seen xs
      | otherwise            = x : go (Set.insert x seen) xs
