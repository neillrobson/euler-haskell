{-# LANGUAGE KindSignatures #-}

module Utils.SingLib where

data DoorState = Opened | Closed | Locked deriving (Eq, Show)

newtype Door (s :: DoorState) = UnsafeMkDoor {doorMaterial :: String}

-- | Unlocks a door, but only if the user enters an odd number as a password.
unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockDoor = undefined
