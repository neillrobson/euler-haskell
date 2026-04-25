{-# LANGUAGE LambdaCase #-}

module Main where

import Solutions.P0048 (solve)
import Solutions.P0069 (solve)
import Solutions.P0070 (solve)
import Solutions.P0097 (solve)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

main :: IO ()
main = do
  problem <- getInteger
  let solution = answer problem
  case solution of
    Nothing -> putStrLn "Not yet solved"
    Just i -> putStrLn $ "Solution: " ++ show i

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

getInteger :: IO Integer
getInteger = do
  line <- prompt "Problem to solve: "
  let maybeInt = readMaybe line :: Maybe Integer
  case maybeInt of
    Nothing -> do
      putStrLn $ "Invalid integer " ++ line
      getInteger
    Just i -> return i

answer :: Integer -> Maybe Integer
answer = \case
  48 -> Just Solutions.P0048.solve
  69 -> Just Solutions.P0069.solve
  70 -> Just Solutions.P0070.solve
  97 -> Just Solutions.P0097.solve
  _ -> Nothing
