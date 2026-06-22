{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

import Data.Kind (Type)

-- Source: https://blog.jle.im/entry/introduction-to-singletons-1.html

data DoorState = Opened | Closed | Locked deriving (Eq, Show)

newtype Door (s :: DoorState) = UnsafeMkDoor {doorMaterial :: String}

-- Equivalent with GADT
data GDoor :: DoorState -> Type where
  UnsafeMkGDoor :: {gDoorMaterial :: String} -> GDoor s

-- Singleton version
data SDoorState :: DoorState -> Type where
  SOpened :: SDoorState 'Opened
  SClosed :: SDoorState 'Closed
  SLocked :: SDoorState 'Locked

--------------------------------------------------------------------------------
-- Usage examples
--------------------------------------------------------------------------------

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor = undefined

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor = undefined

lockAnyDoor :: SDoorState s -> Door s -> Door 'Locked
lockAnyDoor = \case
  SOpened -> lockDoor . closeDoor
  SClosed -> lockDoor
  SLocked -> id
