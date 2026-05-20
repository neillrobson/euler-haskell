module Solutions.P0030 where

solve :: Integer
solve = sum matchingFifthPowers

-- | Produces digits in the opposite of reading order.
digits :: Integer -> [Integer]
digits 0 = []
digits n = (n `mod` 10) : digits (n `div` 10)

fifthPowerSum :: Integer -> Integer
fifthPowerSum = sum . map (^ (5 :: Int)) . digits

-- | The max fifth-power-sum of a seven digit number is only six digits,
-- so we only need to check through six-digit numbers.
maxToCheck :: Integer
maxToCheck = 6 * 9 ^ (5 :: Int)

-- | The number one is skipped since it isn't technically a sum.
matchingFifthPowers :: [Integer]
matchingFifthPowers = filter (\x -> x == fifthPowerSum x) [2 .. maxToCheck]
