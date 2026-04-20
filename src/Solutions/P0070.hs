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
-- So, we start with primes that remain below the limit when squared,
-- and check the anagram condition going down.

solve :: Integer
solve = (^ (2 :: Integer)) $ head $ filter (\p -> (p ^ (2 :: Integer)) `isAnagram` phiPrimeSq p) potentialPrimes

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

potentialPrimes :: [Integer]
potentialPrimes = reverse $ takeWhile (<= floor (sqrt 10000000 :: Double)) primes

-- Return the totient value of the squared prime input.
phiPrimeSq :: Integer -> Integer
phiPrimeSq p = p ^ (2 :: Integer) - p
