{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}

module Test.Units where

import Data.Metrology

data Meter = Meters
data Foot = Feet
data Yard = Yards

type Length = MkQu Meter
type Time = MkQu Second

instance Unit Meter where
  type BaseUnit Meter = Canonical

instance Unit Foot where
  type BaseUnit Foot = Meter
  conversionRatio _ = 0.3048

instance Unit Yard where
  type BaseUnit Yard = Foot
  conversionRatio _ = 3

data Second = Seconds
instance Unit Second where
  type BaseUnit Second = Canonical

data Hertz = Hertz
instance Unit Hertz where
  type BaseUnit Hertz = Number :/ Second
  conversionRatio _ = 1

