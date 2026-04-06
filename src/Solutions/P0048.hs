module Solutions.P0048 where

solve :: Integer
solve = selfPowerSum (10 ^ (10 :: Integer)) 1000

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

sumModM :: Modulus -> [Integer] -> Integer
sumModM m = foldr ((`mod` m) .: (+)) 0

selfPowerSum :: Modulus -> Integer -> Integer
selfPowerSum m x = sumModM m $ map (\i -> modularExp i i m) [1 .. x]

--------------------------------------------------------------------------------

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f $ g x y
