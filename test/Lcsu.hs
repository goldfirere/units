{-# LANGUAGE TypeFamilies, DataKinds #-}

module Test.Lcsu where

import Data.Metrology hiding (LCSU)
import Data.Metrology.Units
import Data.Metrology.Quantity
import Data.Metrology.DimSpec
import Data.Metrology.Z

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

inMeters :: Qu '[D Length One] LCSU Double
inMeters = dimOf 3 Meter
