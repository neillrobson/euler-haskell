{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Solutions.P0070 where

import Data.List (tails)
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

type Power = Integer

type Prime = Integer

type N = Integer

type Phi = Integer

phiPrimePow :: Power -> Prime -> (N, Phi)
phiPrimePow pow pri = (curr, curr - prev)
  where
    prev = pri ^ (pow - 1)
    curr = prev * pri

-- However, `concatMap solutionsAt [2..23]` is empty.

solutionsAt :: Integer -> [(Integer, Integer)]
solutionsAt k = filter (uncurry isAnagram) $ map (phiPrimePow k) $ potentialPrimes k

--------------------------------------------------------------------------------

-- Let's go through pairs of primes next.
-- We've got to get them ordered by the min-condition.

mapPairs :: (a -> a -> a) -> [a] -> [a]
mapPairs f (x : y : zs) = f x y : mapPairs f zs
mapPairs _ zs = zs

treeFold :: (a -> a -> a) -> a -> [a] -> a
treeFold _ z [] = z
treeFold f z (x : xs) = f x $ treeFold f z $ mapPairs f xs

makePairList :: [a] -> [(a, a)]
makePairList [] = []
makePairList xs@(x : _) = map (x,) xs

primePairs :: [Integer] -> [(Integer, Integer)]
primePairs ps = treeFold merge [] $ map makePairList $ init $ tails ps
  where
    merge [] ys = ys
    -- Assume head of x meets the condition better than head of y
    merge (x : xs) ys = x : go xs ys
      where
        go (u : us) (v : vs)
          | uncurry nOverPhiPair u < uncurry nOverPhiPair v = u : go us (v : vs)
          | otherwise = v : go (u : us) vs
        go [] as = as
        go as [] = as

nOverPhiPair :: Integer -> Integer -> Double
nOverPhiPair (fromInteger -> p) (fromInteger -> q) = (p * q) / ((p - 1) * (q - 1))

phiPrimePair :: Prime -> Prime -> (N, Phi)
phiPrimePair p q = (p * q, (p - 1) * (q - 1))

-- This yields 7026037, incorrect.
maybeSolve :: Integer
maybeSolve = fst $ head $ filter (uncurry isAnagram) $ map (uncurry phiPrimePair) $ primePairs $ potentialPrimes 2
