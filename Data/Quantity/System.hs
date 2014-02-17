{-# LANGUAGE DataKinds, GADTs, TemplateHaskell, TypeOperators, TypeFamilies, PolyKinds, ScopedTypeVariables, UndecidableInstances #-}
module Data.Quantity.System where

import Data.Quantity.Zahl
import Data.Ratio
import Data.Singletons.Eq
import Data.Singletons.TH


-- | kind annotation for dimension names
$(singletons [d|
  data DimK star = Dim star
  |] )

-- | kind annotation for unit names
$(singletons [d|
  data UniK star = Uni star
  |] )


type family EqDim a b where
  EqDim (Dim a) (Dim b) = a == b

type instance a == b = EqDim a b

type family DimOf (a::k) :: [(DimK *, Zahl)]
     

class IsDimensionName d where
  type GlobalBaseUnit d :: (UniK *)



type family EqUnit a b where
  EqUnit (Uni a) (Uni b) = a == b

type instance a == b = EqUnit a b

class IsUnit u where
  type DimOfUnit u :: [(DimK *, Zahl)]
  conversionFactor :: u -> Rational

type instance DimOf (Uni u) = DimOfUnit u     

