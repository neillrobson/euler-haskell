{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.Singleton2 where

import Data.Kind (Type)
import Data.Singletons.TH (genSingletons)

data DoorState = Opened | Closed | Locked deriving (Eq, Show)

$(genSingletons [''DoorState])

-- | Technically, @Door@ is three distinct types, indexed by a type of kind
-- @DoorState@.
data Door :: DoorState -> Type where
  UnsafeMkDoor :: {doorMaterial :: String} -> Door s
