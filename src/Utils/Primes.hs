{-# LANGUAGE TupleSections #-}

module Utils.Primes where

import Control.Monad (forM_, when)
import Data.Array.ST (modifyArray', newListArray, readArray, runSTUArray)
import Data.Array.Unboxed (accum, elems, listArray)
import Data.Int (Int64)
import Data.Word (Word64)

--------------------------------------------------------------------------------

primes :: (Integral a) => [a]
primes = 2 : 3 : filter isPrime (chain [5, 11 ..] [7, 13 ..])
  where
    chain [] ys = ys
    chain xs [] = xs
    chain (x : xs) (y : ys) = x : y : chain xs ys

isPrime :: (Integral a) => a -> Bool
isPrime x = all (\i -> x `mod` i /= 0) $ takeWhile (\i -> i * i <= x) primes

-- eulerPhi :: Int64 -> [Int64]
-- eulerPhi limit = elems $ accum (\x p -> x - x `div` p) phi primePairs
--   where
--     ps = takeWhile (<= limit) primes
--     idxs = map (\p -> takeWhile (<= limit) [p, p * 2 ..]) ps
--     primePairs = concat $ zipWith (\is p -> (,p) <$> is) idxs ps
--     phi = listArray (1, limit) [1 .. limit]

--------------------------------------------------------------------------------

phiST :: Word64 -> [Word64]
phiST limit = elems $ runSTUArray $ do
  phi <- newListArray (1, limit) [1 .. limit]
  forM_ [2 .. limit] $ \i -> do
    phii <- readArray phi i
    when (phii == i) $ do
      let js = takeWhile (<= limit) [i, 2 * i ..]
      forM_ js $ \j -> modifyArray' phi j (\e -> e - e `div` i)
  return phi
