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

--------------------------------------------------------------------------------

{-
Plenty fast, but we can do better.
Using p(n) = n^2 + n + 41 as a baseline...

If any property (like primality) holds for 0<=n<=L in p(n),
it'll also hold for those values in p(L-n).

For our parabola in particular (always positive, close to center on x-axis),
shifting to the right gets us up to 2L primes, because there will be duplicates
on each side of the parabolic curve.

Multiply out p(L-n) = (L-n)^2 + (L-n) + 41:
p(L-n) = n^2 - (2L+1)n + (L^2+L+41)

b=L^2+L+41.
L won't be greater than 39 (because of the original formula's limit).
b must be a prime (so n=0 is prime);
b should also be as large as possible (shifting as far right as possible);
Finally, b must be less than or equal to 1000, as per the problem statement.
-}
