module Solutions.P0069 where

import Data.Ratio ((%))
import Data.Word (Word64)
import Utils.Primes (phiST, primes)

-- Fast smart version.
-- phi(n) = n * */ (1 - 1/p)
-- n / phi(n) = 1 / (*/ (1 - 1/p)) = 1 / (*/ (p-1)/p) = */ (p / (p-1))
-- So, we just need to find the number less than the limit with the most distinct primes.
-- Easy enough to do: generate and multiply together the primes, smallest to largest,
-- until we hit the limit.

solve :: Integer
solve = last $ takeWhile (< 1000000) $ scanl (*) 1 primes

--------------------------------------------------------------------------------

-- Slow manual version.
-- Runtime could probably be improved by doing the fold inline with the
-- generation process. However, on my machine, solve runs in <3s; not bad.

manualSolve :: Integer
manualSolve = maxNOverPhi 1000000

phiWithIndices :: Word64 -> [(Integer, Word64)]
phiWithIndices limit = zip [1 .. toInteger limit] $ phiST limit

maxNOverPhi :: Word64 -> Integer
maxNOverPhi =
  fst
    . foldl'
      ( \maxPair@(_, maxVal) (i, v) ->
          let nextPair@(_, nextVal) = (i, i % toInteger v)
           in if nextVal > maxVal then nextPair else maxPair
      )
      (0, 0)
    . phiWithIndices
