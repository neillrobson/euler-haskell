module Solutions.P0069 where

import Data.Foldable (Foldable (foldl'))
import Data.Ratio ((%))
import Data.Word (Word64)
import Utils.Primes (phiST)

solve :: Integer
solve = maxNOverPhi 1000000

--------------------------------------------------------------------------------

phiWithIndices :: Word64 -> [(Integer, Word64)]
phiWithIndices limit = zip [1 .. toInteger limit] $ phiST limit

maxNOverPhi :: Word64 -> Integer
maxNOverPhi limit =
  fst
    $ foldl'
      ( \maxPair@(_, maxVal) (i, v) ->
          let nextPair@(_, nextVal) = (i, i % toInteger v)
           in if nextVal > maxVal then nextPair else maxPair
      )
      (0, 0)
    $ phiWithIndices limit
