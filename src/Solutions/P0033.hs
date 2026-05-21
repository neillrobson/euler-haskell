{-# LANGUAGE TupleSections #-}

module Solutions.P0033 where

solve :: Integer
solve = undefined

toCheck :: [Integer]
toCheck = filter (\x -> x `mod` 11 /= 0) $ (+) <$> [10, 20 .. 90] <*> [1 .. 9]

subsets :: [a] -> [(a, a)]
subsets [] = []
subsets (x : xs) = map (x,) xs ++ subsets xs
