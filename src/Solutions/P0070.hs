{-# LANGUAGE TupleSections #-}

module Solutions.P0070 where

import qualified Data.Map as M
import Data.Word (Word64)
import Solutions.P0069 (phiWithIndices)

solve :: Integer
solve = undefined

--------------------------------------------------------------------------------

isAnagram :: Integer -> Integer -> Bool
isAnagram = go $ M.fromList $ map (,0 :: Integer) [0 .. 9]
  where
    go s a b
      | a == 0 && b /= 0 = False
      | a /= 0 && b == 0 = False
      | a == 0 && b == 0 = all (== 0) s
      | otherwise =
          let (ad, am) = a `divMod` 10
              (bd, bm) = b `divMod` 10
              s' = M.adjust (subtract 1) bm $ M.adjust (+ 1) am s
           in go s' ad bd

filteredPhi :: Word64 -> [(Integer, Word64)]
filteredPhi = filter (\(i, v) -> isAnagram i $ toInteger v) . phiWithIndices
