{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Utils.SingLib where

import Data.Singletons.TH

data DoorState = Opened | Closed | Locked deriving (Eq, Show)

$(genSingletons [''DoorState])

newtype Door (s :: DoorState) = UnsafeMkDoor {doorMaterial :: String}

-- | Unlocks a door, but only if the user enters an odd number as a password.
unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockDoor i (doorMaterial -> m)
  | odd i = Just $ UnsafeMkDoor m
  | otherwise = Nothing
