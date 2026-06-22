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

-- Note how @closeDoor@ and @lockDoor@ have the exact same term-level definition

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor m) = UnsafeMkDoor m

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

-- | The indirection and type signature for @go@ is necessary to bind the same
-- state parameter between the @SDoorState@ and @Door@.
lockAnyDoor'' :: (SingDSI s) => Door s -> Door 'Locked
lockAnyDoor'' = go singDS
  where
    go :: SDoorState t -> Door t -> Door 'Locked
    go SOpened = lockDoor . closeDoor
    go SClosed = lockDoor
    go SLocked = id

mkDoor :: SDoorState s -> String -> Door s
mkDoor _ = UnsafeMkDoor

mkDoor' :: (SingDSI s) => String -> Door s
mkDoor' = mkDoor singDS
