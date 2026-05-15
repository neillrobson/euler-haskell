module Solutions.P0027 where

import Data.Foldable (maximumBy)
import Data.Function (on)
import Utils.Primes (isPrime)

solve :: Integer
solve = fromIntegral $ a * b
  where
    (a, b) = longestConsecWithin 1000

pairs :: Int -> [(Int, Int)]
pairs n = [(a, b) | a <- [negate n + 1 .. n - 1], b <- [negate n .. n]]

quad :: (Int, Int) -> Int -> Int
quad (a, b) n = n * n + a * n + b

consecPrimes :: (Int, Int) -> Int
consecPrimes ab = length $ takeWhile isPrime $ map (quad ab) [0 ..]

longestConsecWithin :: Int -> (Int, Int)
longestConsecWithin = fst . maximumBy (compare `on` snd) . map (\ab -> (ab, consecPrimes ab)) . pairs
