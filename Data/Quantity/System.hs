{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, PolyKinds, UndecidableInstances #-}
module Data.Quantity.System where

import Data.Singletons.Eq
import Data.Quantity.Zahl

-- | kind annotation for dimension names
data DimK star = Dim star

type family EqDim a b where
  EqDim (Dim a) (Dim b) = a == b

type instance a == b = EqDim a b

type family DimOf (a::k) :: [(DimK *, Zahl)]
     
class IsDimName d where
  type GlobalBaseUnit d :: (UnitK *)



-- | kind annotation for unit names
data UnitK star = Unit star

type family EqUnit a b where
  EqUnit (Unit a) (Unit b) = a == b

type instance a == b = EqUnit a b

class IsUnitName u where
  type DimOfUnitName u :: [(DimK *, Zahl)]

type instance DimOf (Unit u) = DimOfUnitName u     