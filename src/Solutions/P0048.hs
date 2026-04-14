module Solutions.P0048 where

import Utils.Modular (Mod (Mod, unMod))

solve :: Integer
solve = unMod $ selfPowerSum 1000

--------------------------------------------------------------------------------

type ModTenTen = Mod 10000000000

type Base = ModTenTen

type Exponent = Integer

type Result = ModTenTen

modularExp :: Base -> Exponent -> Result
modularExp _ 0 = 1
modularExp b e =
  let b' = b ^ (2 :: Integer)
      e' = e `div` 2
      c = if odd e then b else 1
   in c * modularExp b' e'

selfPowerSum :: Integer -> Result
selfPowerSum x = sum $ map (\i -> modularExp (Mod i) i) [1 .. x]
