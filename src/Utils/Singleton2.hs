{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.Singleton2 where

import Data.Kind (Type)
import Data.Singletons.TH (Sing, SingI (sing), genSingletons)

data DoorState = Opened | Closed | Locked deriving (Eq, Show)

$(genSingletons [''DoorState])

-- | Technically, @Door@ is three distinct types, indexed by a type of kind
-- @DoorState@.
data Door :: DoorState -> Type where
  UnsafeMkDoor :: {doorMaterial :: String} -> Door s

--------------------------------------------------------------------------------
-- Existential datatypes
--------------------------------------------------------------------------------

data SomeDoor :: Type where
  MkSomeDoor :: Sing s -> Door s -> SomeDoor

fromDoor :: Sing s -> Door s -> SomeDoor
fromDoor = MkSomeDoor

fromDoor_ :: (SingI s) => Door s -> SomeDoor
fromDoor_ = fromDoor sing

--------------------------------------------------------------------------------
-- Some old definitions
--------------------------------------------------------------------------------

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor m) = UnsafeMkDoor m

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockAnyDoor :: SDoorState s -> Door s -> Door 'Locked
lockAnyDoor = \case
  SOpened -> lockDoor . closeDoor
  SClosed -> lockDoor
  SLocked -> id

--------------------------------------------------------------------------------
-- Interoperability
--------------------------------------------------------------------------------

closeSomeOpenedDoor :: SomeDoor -> Maybe SomeDoor
closeSomeOpenedDoor (MkSomeDoor s d) = case s of
  SOpened -> Just $ fromDoor_ d
  SClosed -> Nothing
  SLocked -> Nothing

lockAnySomeDoor :: SomeDoor -> SomeDoor
lockAnySomeDoor (MkSomeDoor s d) = fromDoor_ $ lockAnyDoor s d
