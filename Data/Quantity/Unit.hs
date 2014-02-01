module Data.Quantity.Unit where

-- kind annotation for base units
data BaseUnitK star = BaseUnit star

type family EqBaseUnit a b where
  EqBaseUnit (BaseUnit a) (BaseUnit b) = a == b

type instance a == b = EqBaseUnit a b