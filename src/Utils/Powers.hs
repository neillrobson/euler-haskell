module Utils.Powers where

import Data.Bits (Bits (shiftR, testBit))

-- | Recursive binary exponentiation. O(lg e).
bExp :: (Num a, Integral e) => a -> e -> a
bExp _ 0 = 1
bExp b e =
  let b' = b * b
      e' = e `div` 2
      c = if odd e then b else 1
   in c * bExp b' e'

-- | Recursive binary multiplication. O(lg n).
bMul :: (Num a, Bits a) => a -> a -> a
bMul _ 0 = 0
bMul m n =
  let m' = m + m
      n' = shiftR n' 1
      c = if bOdd n then m else 0
   in c + bMul m' n'

-- | Check if the given Bits value is odd.
bOdd :: (Bits a) => a -> Bool
bOdd = flip testBit 0
