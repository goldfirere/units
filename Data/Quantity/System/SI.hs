{-# LANGUAGE DataKinds, TypeFamilies #-}
module Data.Quantity.System.SI where

import Data.Quantity.System
import Data.Quantity.Zahl
import Data.Singletons

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
data KiloMeter = KiloMeter


data instance Sing Meter = SMeter
instance SingI Meter where sing = sing

instance IsUnitName Meter where
  type DimOfUnitName Meter = '[ '(Dim Length, Posi 1) ]
  conversionFactorOfName _ = 1
  

data instance Sing KiloMeter = SKiloMeter
instance SingI KiloMeter where sing = sing

instance IsUnitName KiloMeter where
  type DimOfUnitName KiloMeter = '[ '(Dim Length, Posi 1) ]
  conversionFactorOfName _ = 1000


instance IsUnitName Second where
  type DimOfUnitName Second = '[ '(Dim Time, Posi 1) ]
  conversionFactorOfName _ = 1

