{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad ((>=>))
import Data.Maybe (listToMaybe)
import Solutions (buildDispatcher)
import Solutions.P0001 (solve)
import Solutions.P0025 (solve)
import Solutions.P0026 (solve)
import Solutions.P0048 (solve)
import Solutions.P0069 (solve)
import Solutions.P0070 (solve)
import Solutions.P0072 (solve)
import Solutions.P0073 (solve)
import Solutions.P0097 (solve)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

main :: IO ()
main = do
  problem <- getProblem
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

getProblem :: IO Integer
getProblem = do
  args <- getArgs
  let maybeProblem = (listToMaybe >=> readMaybe) args :: Maybe Integer
  maybe getInteger return maybeProblem

answer :: Integer -> Maybe Integer
answer = $(buildDispatcher "src/Solutions")
