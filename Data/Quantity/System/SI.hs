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
newtype Kilo a = Kilo a


data instance Sing Meter = SMeter
instance SingI Meter where sing = SMeter

instance IsUnitName Meter where
  type DimOfUnitName Meter = '[ '(Dim Length, Posi 1) ]
  conversionFactorOfName _ = 1
  

data instance Sing KiloMeter = SKiloMeter
instance SingI KiloMeter where sing = SKiloMeter

instance IsUnitName KiloMeter where
  type DimOfUnitName KiloMeter = '[ '(Dim Length, Posi 1) ]
  conversionFactorOfName _ = 1000


data instance Sing (Kilo a) = SKilo (Sing a)
instance SingI a => SingI (Kilo a) where sing = SKilo sing

instance IsUnitName a => IsUnitName (Kilo a) where
  type DimOfUnitName (Kilo a) = DimOfUnitName a
  conversionFactorOfName (Kilo x) = 1000 
    * conversionFactorOfName x



instance IsUnitName Second where
  type DimOfUnitName Second = '[ '(Dim Time, Posi 1) ]
  conversionFactorOfName _ = 1

