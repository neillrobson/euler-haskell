{-# LANGUAGE TupleSections #-}

module Solutions.P0070 where

import qualified Data.Map as M
import Utils.Primes (primes)

-- phi(n) = n * */ (1 - 1/p)
-- n / phi(n) = 1 / (*/ (1 - 1/p)) = 1 / (*/ (p-1)/p) = */ (p / (p-1))
-- This time, we need the number with the fewest distinct primes,
-- with larger primes being better.
-- The anagram condition also must hold.
-- Any true prime has a phi of (p-1) which is definitely not an anagram.

solve :: Integer
solve = undefined

--------------------------------------------------------------------------------

limit :: Integer
limit = 10000000

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

-- So, we start with primes that remain below the limit when raised to a power,
-- and check the anagram condition going down.

potentialPrimes :: Integer -> [Integer]
potentialPrimes pow = reverse $ takeWhile (<= floor ((** (1 / fromInteger pow)) $ fromInteger limit :: Double)) primes

phiPrimePow :: Integer -> Integer -> (Integer, Integer)
phiPrimePow pow pri = (curr, curr - prev)
  where
    prev = pri ^ (pow - 1)
    curr = prev * pri

-- However, `concatMap solutionsAt [2..23]` is empty.

solutionsAt :: Integer -> [(Integer, Integer)]
solutionsAt k = filter (uncurry isAnagram) $ map (phiPrimePow k) $ potentialPrimes k
