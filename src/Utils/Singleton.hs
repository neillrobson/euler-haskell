{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Utils.Singleton where

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
-- Usage examples, explicit passing of singleton
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

fromSDoorState :: SDoorState s -> DoorState
fromSDoorState SOpened = Opened
fromSDoorState SClosed = Closed
fromSDoorState SLocked = Locked

doorStatus :: SDoorState s -> Door s -> DoorState
doorStatus state _ = fromSDoorState state

--------------------------------------------------------------------------------
-- Implicit singleton passing
--------------------------------------------------------------------------------

class SingDSI s where
  singDS :: SDoorState s

instance SingDSI 'Opened where
  singDS = SOpened

instance SingDSI 'Closed where
  singDS = SClosed

instance SingDSI 'Locked where
  singDS = SLocked

lockAnyDoor' :: (SingDSI s) => Door s -> Door 'Locked
lockAnyDoor' = lockAnyDoor singDS

doorStatus' :: (SingDSI s) => Door s -> DoorState
doorStatus' = doorStatus singDS

-- This doesn't work because we don't know which `singDS` instance to choose.
-- `SingDSI s` binds an `s` at the type level, but `singDS` is working at the
-- term level. The error hints at this by referring to `s0`, a different `s`.
-- lockAnyDoor'' :: (SingDSI s) => Door s -> Door 'Locked
-- lockAnyDoor'' = case singDS of
--   SOpened -> lockDoor . closeDoor
--   SClosed -> lockDoor
--   SLocked -> id

mkDoor :: SDoorState s -> String -> Door s
mkDoor _ = UnsafeMkDoor

mkDoor' :: (SingDSI s) => String -> Door s
mkDoor' = mkDoor singDS
