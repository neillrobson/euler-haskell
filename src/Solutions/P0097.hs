module Solutions.P0097 where

import Solutions.P0048 (Modulus, modularExp)

solve :: Integer
solve = (powerOfTwo * 28433 + 1) `mod` modulus

--------------------------------------------------------------------------------

modulus :: Modulus
modulus = 10 ^ (10 :: Integer)

powerOfTwo :: Integer
powerOfTwo = modularExp 2 7830457 modulus
