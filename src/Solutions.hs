{-# LANGUAGE TemplateHaskellQuotes #-}

module Solutions where

import Data.Maybe (catMaybes)
import Data.Traversable (for)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Quasi (qAddDependentFile))
import System.Directory (listDirectory)
import System.FilePath (takeBaseName)
import Text.Read (readMaybe)

buildDispatcher :: FilePath -> Q Exp
buildDispatcher dir = do
  -- hpack should update this whenever solution files are added/removed
  qAddDependentFile "euler-haskell.cabal"
  files <- runIO $ listDirectory dir
  let problems =
        [ n
          | f <- files,
            let base = takeBaseName f,
            'P' : rest <- [base],
            Just n <- [readMaybe rest :: Maybe Integer]
        ]
  arg <- newName "n"
  rawMatches <- mapM mkMatch problems
  let matches = map pure $ catMaybes rawMatches
  let fallback = match wildP (normalB [|Nothing|]) []
  lamE [varP arg] (caseE (varE arg) (matches ++ [fallback]))
  where
    mkMatch n = do
      let modName = "Solutions.P" ++ pad4 n
          funcName = modName ++ ".solve"
      solveNm <- lookupValueName funcName
      for solveNm $ \solveName -> do
        match (litP (IntegerL n)) (normalB [|Just $(varE solveName)|]) []
    pad4 n = replicate (4 - length s) '0' ++ s where s = show n
