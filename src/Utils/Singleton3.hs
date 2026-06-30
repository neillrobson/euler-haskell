{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Singleton3 where

import Data.Kind (Type)
import Data.Singletons.TH (Sing, genSingletons)

data DoorState = Opened | Closed | Locked deriving (Eq, Show)

$(genSingletons [''DoorState])

type Door :: DoorState -> Type
data Door s where
  UnsafeMkDoor :: {doorMaterial :: String} -> Door s

mkDoor :: Sing s -> String -> Door s
mkDoor _ = UnsafeMkDoor
