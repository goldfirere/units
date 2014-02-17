{-# LANGUAGE DataKinds, TypeFamilies #-}
module Data.Quantity.System.SI where

import Data.Quantity.System
import Data.Quantity.Zahl
import Data.Singletons

data Length = Length
data instance Sing Length = SLength
instance SingI Length where sing = SLength


data Time = Time
data Mass = Mass
data instance Sing Mass = SMass
instance SingI Mass where sing = SMass
data Current = Current
data Temperature = Temperature

instance IsDimensionName Length where
  type GlobalBaseUnit Length = Uni Meter

instance IsDimensionName Time where
  type GlobalBaseUnit Time = Uni Second

instance IsDimensionName Mass where
  type GlobalBaseUnit Mass = Uni (Kilo Gram)


data Meter = Meter
data Second = Second
data Gram = Gram
newtype Kilo a = Kilo a
newtype Centi a = Centi a


data instance Sing Meter = SMeter
instance SingI Meter where sing = SMeter

data instance Sing Second = SSecond
instance SingI Second where sing = SSecond                           

data instance Sing Gram = SGram
instance SingI Gram where sing = SGram                           

instance IsUnitName Meter where
  type DimOfUnitName Meter = '[ '(Dim Length, Posi 1) ]
  conversionFactorOfName _ = 1


instance IsUnitName Second where
  type DimOfUnitName Second = '[ '(Dim Time, Posi 1) ]
  conversionFactorOfName _ = 1


instance IsUnitName Gram where
  type DimOfUnitName Gram = '[ '(Dim Mass, Posi 1) ]
  conversionFactorOfName _ = 1


data instance Sing (Centi a) = SCenti (Sing a)
instance SingI a => SingI (Centi a) where sing = SCenti sing

instance IsUnitName a => IsUnitName (Centi a) where
  type DimOfUnitName (Centi a) = DimOfUnitName a
  conversionFactorOfName (Centi x) = 0.01
    * conversionFactorOfName x

data instance Sing (Kilo a) = SKilo (Sing a)
instance SingI a => SingI (Kilo a) where sing = SKilo sing

instance IsUnitName a => IsUnitName (Kilo a) where
  type DimOfUnitName (Kilo a) = DimOfUnitName a
  conversionFactorOfName (Kilo x) = 1000 
    * conversionFactorOfName x




