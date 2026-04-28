{-# LANGUAGE TemplateHaskellQuotes #-}

module Solutions where

import Language.Haskell.TH

-- import Language.Haskell.TH.Syntax (Quasi (qAddDependentFile))
-- import System.Directory (listDirectory)

buildDispatcher :: FilePath -> Q Exp
buildDispatcher _ = do
  -- qAddDependentFile dir
  -- files <- runIO $ listDirectory dir
  arg <- newName "n"
  hardcoded <- match (litP (IntegerL 69)) (normalB [|Just $(varE (mkName "Solutions.P0069.solve"))|]) []
  fallback <- match wildP (normalB [|Nothing|]) []
  lamE [varP arg] (caseE (varE arg) [pure hardcoded, pure fallback])
