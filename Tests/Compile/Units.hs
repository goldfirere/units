{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}

module Tests.Compile.Units where

import Data.Metrology

data Meter = Meters
data Foot = Feet
data Yard = Yards

type Length = MkQu_U Meter
type Time = MkQu_U Second

data LengthD = LengthD
instance Dimension LengthD
data TimeD = TimeD
instance Dimension TimeD

instance Unit Meter where
  type BaseUnit Meter = Canonical
  type DimOfUnit Meter = LengthD

instance Unit Foot where
  type BaseUnit Foot = Meter
  conversionRatio _ = 0.3048

instance Unit Yard where
  type BaseUnit Yard = Foot
  conversionRatio _ = 3

data Second = Seconds
instance Unit Second where
  type BaseUnit Second = Canonical
  type DimOfUnit Second = TimeD

data Hertz = Hertz
instance Unit Hertz where
  type BaseUnit Hertz = Number :/ Second
  conversionRatio _ = 1

