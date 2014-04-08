{-# LANGUAGE TypeFamilies, DataKinds #-}

module Test.Lcsu where

import Data.Dimensions
import Data.Dimensions.Units
import Data.Dimensions.Dim
import Data.Dimensions.DimSpec
import Data.Dimensions.Z

data Length = Length
data Time = Time

data Meter = Meter
data Second = Second

data Centi = Centi

instance Dimension Length
instance Dimension Time

instance Unit Meter where
  type BaseUnit Meter = Canonical
  type DimOfUnit Meter = Length
instance Unit Second where
  type BaseUnit Second = Canonical
  type DimOfUnit Second = Time

instance Unit Centi where
  type BaseUnit Centi = Meter
  conversionRatio _ = 0.01

data Foot = Foot
instance Unit Foot where
  type BaseUnit Foot = Meter
  conversionRatio _ = 0.3048

type LCSU = MkLCSU '[(Length, Meter),  (Time, Second)]

inMeters :: Dim Double '[D Length One] LCSU
inMeters = dimOf 3 Meter
