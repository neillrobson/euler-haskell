module Solutions.P0001 where

solve :: Integer
solve = sumMultsBelow 3 1000 + sumMultsBelow 5 1000 - sumMultsBelow 15 1000

sumMultsBelow :: Integer -> Integer -> Integer
sumMultsBelow k n = k * gauss ((n - 1) `div` k)

gauss :: Integer -> Integer
gauss n = n * (n + 1) `div` 2
