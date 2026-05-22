module Solutions.P0034 where

import Data.Array.Unboxed (UArray, listArray, (!))

solve :: Integer
solve = fromIntegral $ sum $ filter (\x -> x == digitFactorialSum x) [3 .. 2540160]

-- | The factorial numbers from zero to nine.
factorials :: UArray Int Int
factorials = listArray (0, 9) $ scanl (*) 1 [1 .. 9]

-- | Produces digits in the opposite of reading order.
digits :: Int -> [Int]
digits 0 = []
digits n = (n `mod` 10) : digits (n `div` 10)

digitFactorialSum :: Int -> Int
digitFactorialSum = sum . map (factorials !) . digits
