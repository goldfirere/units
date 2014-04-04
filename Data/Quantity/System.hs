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

type family DimOf (a::k) :: [(DimK *, Zahl)]

type family EqDim a b where
  EqDim (Dim a) (Dim b) = a == b
type instance a == b = EqDim a b

-- | kind annotation for unit names
$(singletons [d|
  data UniK star = Uni star
  |] )

type family UniOf (a::k) :: [(UniK *, Zahl)]
     
type family EqUnit a b where
  EqUnit (Uni a) (Uni b) = a == b
type instance a == b = EqUnit a b


     
class IsDimensionName d where
  type GlobalBaseUnit d :: (UniK *)


class IsUnitName u where
  type DimOfUnitName u :: [(DimK *, Zahl)]
  -- | conversion factor to @u@ from the global base unit of the same dimension as @u@.
  --   for example, conversion factor of centimeter in SI is 0.01 .       
  conversionFactorOfName :: u -> Rational


class SingI u => IsUnit (u :: k) where
  type DimOfUnit u :: [(DimK *, Zahl)]
  -- | conversion factor to @u@ from the global base unit of the same dimension as @u@.
  --   for example, conversion factor of centimeter in SI is 0.01 .       
  conversionFactor :: Sing u -> Rational

instance IsUnit ('[] :: [(UniK *, Zahl)]  ) where
  type DimOfUnit ('[] :: [(UniK *, Zahl)]  )  = '[]
  conversionFactor _ = 1

type instance DimOf (a :: [(UniK *, Zahl)]) = DimOfUnit a
  

-- dummy function for the invocation of the TH
dimOfUnit :: [(UniK a, Zahl)] -> [(DimK a, Zahl)] 
dimOfUnit = undefined

$( promoteOnly [d|
  zahlPowerOfDim :: Zahl -> [(DimK a, Zahl)] -> [(DimK a, Zahl)]    
  zahlPowerOfDim (Posi 0) d = []              
  zahlPowerOfDim (Nega 0) d = []              
  zahlPowerOfDim (Posi 1) d = d
  zahlPowerOfDim (Nega 1) d = negateMap d              
  zahlPowerOfDim (Posi n) d = addMap d (zahlPowerOfDim (pred (Posi n)) d)
  zahlPowerOfDim (Nega n) d = subMap (zahlPowerOfDim (succ (Nega n)) d) d
  |] )


instance (IsUnitName u, SingI '( 'Uni u, n), SingI n) => IsUnit ( '( 'Uni u, n) :: (UniK *, Zahl))  where
  type DimOfUnit '( 'Uni u, n)  = ZahlPowerOfDim n (DimOfUnitName u) 
  conversionFactor _ = (conversionFactorOfName (error "IsUnit/Term" :: u)) ^^ (fromSing (sing :: Sing n))



                 
instance (IsUnit uh, IsUnit ut) => IsUnit (uh ': ut) where
  type DimOfUnit (uh ': ut) = AddMap (DimOfUnit uh) (DimOfUnit ut)
  conversionFactor _ = conversionFactor (error "IsUnit/List" :: Sing uh) * conversionFactor (undefined :: Sing ut)

$( promoteOnly [d|
  lookupCSU :: [(DimK a, UniK a)] -> [(UniK a, Zahl)] -> [(UniK a, Zahl)] 
  lookupCSU duMap uzMap = lookupCSUWithDim duMap (dimOfUnit uzMap)

  lookupCSUWithDim :: [(DimK a, UniK a)] -> [(DimK a, Zahl)] -> [(UniK a, Zahl)] 
  lookupCSUWithDim duMap [] = []
  lookupCSUWithDim duMap ((d,z):ts) = lookupCSU_aux duMap z ts (lookup d duMap)

  lookupCSU_aux :: [(DimK a, UniK a)] -> Zahl  -> [(DimK a, Zahl)] -> Maybe (UniK a) -> [(UniK a, Zahl)] 
  lookupCSU_aux duMap z ts (Just u) = (u,z) : lookupCSUWithDim duMap ts
  |] )