{-# LANGUAGE DataKinds, TypeFamilies #-}
module Data.Quantity.System.SI where

import Data.Quantity.System
import Data.Quantity.Zahl

data Length = Length
data Time = Time
data Mass = Mass
data Current = Current
data Temperature = Temperature

instance IsDimensionName Length where
  type GlobalBaseUnit Length = Uni Meter

instance IsDimensionName Time where
  type GlobalBaseUnit Time = Uni Second

data Meter = Meter
data Second = Second


instance IsUnitName Meter where
  type DimOfUnitName Meter = '[ '(Dim Length, Posi 1) ]
  conversionFactorOfName _ = 1
  

