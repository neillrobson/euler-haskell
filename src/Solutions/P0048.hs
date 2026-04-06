module Solutions.P0048 where

solve :: Integer
solve = undefined

--------------------------------------------------------------------------------

type Base = Integer

type Exponent = Integer

type Modulus = Integer

modularExp :: Base -> Exponent -> Modulus -> Integer
modularExp _ _ 1 = 0
modularExp _ 0 _ = 1
modularExp b e m =
  let b' = (b ^ (2 :: Integer)) `mod` m
      e' = e `div` 2
      c = if odd e then b `mod` m else 1
   in (c * modularExp b' e' m) `mod` m
