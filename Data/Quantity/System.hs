{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, PolyKinds, UndecidableInstances #-}
module Data.Quantity.System where

import Data.Singletons.Eq
import Data.Quantity.Zahl

-- kind annotation for dimension names
data DimK star = Dim star

type family EqDim a b where
  EqDim (Dim a) (Dim b) = a == b

type instance a == b = EqDim a b

type family DimOf (a::k) :: [(DimK *, Zahl)]
     
class IsDimensionName d where
  type GlobalBaseUnit d :: (UniK *)



-- kind annotation for unit names
data UniK star = Uni star

type family EqUnit a b where
  EqUnit (Uni a) (Uni b) = a == b

type instance a == b = EqUnit a b

class IsUnitName u where
  type DimOfUnitName u :: [(DimK *, Zahl)]

type instance DimOf (Uni u) = DimOfUnitName u     