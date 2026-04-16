{-# LANGUAGE TupleSections #-}

module Utils.Primes where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Int (Int64)
import Data.STRef (modifySTRef, newSTRef, readSTRef)
import GHC.Arr (accum, elems, listArray)

--------------------------------------------------------------------------------

primes :: (Integral a) => [a]
primes = 2 : 3 : filter isPrime (chain [5, 11 ..] [7, 13 ..])
  where
    chain [] ys = ys
    chain xs [] = xs
    chain (x : xs) (y : ys) = x : y : chain xs ys

isPrime :: (Integral a) => a -> Bool
isPrime x = all (\i -> x `mod` i /= 0) $ takeWhile (\i -> i * i <= x) primes

eulerPhi :: Int64 -> [Int64]
eulerPhi limit = elems $ accum (\x p -> x - x `div` p) phi primePairs
  where
    ps = takeWhile (<= limit) primes
    idxs = map (\p -> takeWhile (<= limit) [p, p * 2 ..]) ps
    primePairs = concat $ zipWith (\is p -> (,p) <$> is) idxs ps
    phi = listArray (1, limit) [1 .. limit]

--------------------------------------------------------------------------------

phiST :: (Num a) => [a] -> a
phiST xs = runST $ do
  n <- newSTRef 0
  forM_ xs $ \x -> do
    modifySTRef n (+ x)
  readSTRef n
