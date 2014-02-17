{-# LANGUAGE DataKinds, FlexibleInstances, FlexibleContexts, GADTs, KindSignatures, TemplateHaskell, TypeOperators, TypeFamilies, PolyKinds, ScopedTypeVariables, UndecidableInstances #-}
module Data.Quantity.System where

import Data.Quantity.Map
import Data.Quantity.Zahl
import Data.Ratio
import Data.Singletons.Eq
import Data.Singletons.TH

import Prelude hiding (lookup)

-- | kind annotation for dimension names
$(singletons [d|
  data DimK star = Dim star
  |] )

type family EqDim a b where
  EqDim (Dim a) (Dim b) = a == b
type instance a == b = EqDim a b

-- | kind annotation for unit names
$(singletons [d|
  data UniK star = Uni star
  |] )

type family EqUnit a b where
  EqUnit (Uni a) (Uni b) = a == b
type instance a == b = EqUnit a b


type family DimOf (a::k) :: [(DimK *, Zahl)]
type family UniOf (a::k) :: [(UniK *, Zahl)]     

     
class IsDimensionName d where
  type GlobalBaseUnit d :: (UniK *)


class IsUnitName u where
  type DimOfUnitName u :: [(DimK *, Zahl)]
  -- | conversion factor to @u@ from the global base unit of the same dimension as @u@.
  --   for example, conversion factor of centimeter in SI is 0.01 .       
  conversionFactorOfName :: u -> Rational


class SingI u => IsUnit u where
  type DimOfUnit u :: [(DimK *, Zahl)]
  -- | conversion factor to @u@ from the global base unit of the same dimension as @u@.
  --   for example, conversion factor of centimeter in SI is 0.01 .       
  conversionFactor :: Sing u -> Rational

type instance DimOf (Uni u) = DimOfUnit u     

instance IsUnit ('[] :: [(UniK *, Zahl)]  ) where
  type DimOfUnit ('[] :: [(UniK *, Zahl)]  )  = '[]
  conversionFactor _ = 1

-- dummy function for the invocation of the TH
dimOfUnit :: [(UniK a, Zahl)] -> [(DimK a, Zahl)] 
dimOfUnit = undefined

instance (IsUnitName u, SingI (UniK u, Zahl)) => IsUnit (UniK u, Zahl)  where
  type DimOfUnit (UniK u, Zahl)  = DimOfUnitName u
  conversionFactor _ = conversionFactorOfName (undefined :: u)

                 
instance (IsUnit uh, IsUnit ut) => IsUnit (uh ': ut) where
  type DimOfUnit (uh ': ut) = AddMap (DimOfUnit uh) (DimOfUnit ut)
  conversionFactor _ = conversionFactor (undefined :: Sing uh) * conversionFactor (undefined :: Sing ut)

$( promoteOnly [d|
  lookupLCSU :: [(DimK a, UniK a)] -> [(UniK a, Zahl)] -> [(UniK a, Zahl)] 
  lookupLCSU duMap uzMap = lookupLCSU_aux duMap (dimOfUnit uzMap)

  lookupLCSU_aux :: [(DimK a, UniK a)] -> [(DimK a, Zahl)] -> [(UniK a, Zahl)] 
  lookupLCSU_aux duMap [] = []
  lookupLCSU_aux duMap ((d,z):ts) = lookupLCSU_aux2 duMap z ts (lookup d duMap)

  lookupLCSU_aux2 :: [(DimK a, UniK a)] -> Zahl  -> [(DimK a, Zahl)] -> Maybe (UniK a) -> [(UniK a, Zahl)] 
  lookupLCSU_aux2 duMap z ts (Just u) = (u,z) : lookupLCSU_aux duMap ts
   |] )