module Utils.Powers where

-- | Recursive binary exponentiation. O(lg e).
bExp :: (Num a, Integral e) => a -> e -> a
bExp _ 0 = 1
bExp b e =
  let b' = b * b
      e' = e `div` 2
      c = if odd e then b else 1
   in c * bExp b' e'

-- | Recursive binary multiplication. O(lg n).
bMul :: (Integral a) => a -> a -> a
bMul _ 0 = 0
bMul m n =
  let m' = m + m
      (n', r) = n `divMod` 2
      c = if r == 1 then m else 0
   in c + bMul m' n'
