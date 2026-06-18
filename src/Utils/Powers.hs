module Utils.Powers where

-- | Recursive binary exponentiation. O(lg e).
bExp :: (Num a, Integral e) => a -> e -> a
bExp _ 0 = 1
bExp b e =
  let b' = b * b
      e' = e `div` 2
      c = if odd e then b else 1
   in c * bExp b' e'
