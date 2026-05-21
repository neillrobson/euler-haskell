{-# LANGUAGE TupleSections #-}

module Solutions.P0033 where

import Data.Ratio (denominator, (%))

solve :: Integer
solve = denominator $ a % b
  where
    (a, b) = foldl1 timesTuple $ filter doesCancel $ subsets toCheck

toCheck :: [Integer]
toCheck = filter (\x -> x `mod` 11 /= 0) $ (+) <$> [10, 20 .. 90] <*> [1 .. 9]

subsets :: [a] -> [(a, a)]
subsets [] = []
subsets (x : xs) = map (x,) xs ++ subsets xs

digits :: Integer -> (Integer, Integer)
digits = (`divMod` 10)

doesCancel :: (Integer, Integer) -> Bool
doesCancel (a, b) = (ad == bn && ab == anbd) || (an == bd && ab == adbn)
  where
    ab = a % b
    (an, ad) = digits a
    (bn, bd) = digits b
    anbd = an % bd
    adbn = ad % bn

timesTuple :: (Num a) => (a, a) -> (a, a) -> (a, a)
timesTuple (a, b) (c, d) = (a * c, b * d)
