module Solutions.P0025 where

solve :: Integer
solve = findFibWithDigitCount 1000

findFibWithDigitCount :: Integer -> Integer
findFibWithDigitCount n = go 0 (1, 1)
  where
    go i (a, b) = if n <= digitCount a then i else go (i + 1) (b, a + b)

digitCount :: Integer -> Integer
digitCount = go 1
  where
    go i x = if x > 10 then go (i + 1) (x `div` 10) else i
