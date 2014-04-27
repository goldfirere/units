{- Data/Metrology/Combinators.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file defines combinators to build more complex units and dimensions from simpler ones.
-}

{-# LANGUAGE TypeOperators, TypeFamilies, UndecidableInstances, 
             ScopedTypeVariables, DataKinds, FlexibleInstances,
             ConstraintKinds #-}

module Data.Metrology.Combinators where

import Data.Singletons ( Sing, SingI, sing )

import Data.Metrology.Dimensions
import Data.Metrology.Units
import Data.Metrology.Factor
import Data.Metrology.Z
import Data.Type.Equality
import Data.Metrology.LCSU

infixl 7 :*
-- | Multiply two units to get another unit.
-- For example: @type MetersSquared = Meter :* Meter@
data u1 :* u2 = u1 :* u2

instance (Dimension d1, Dimension d2) => Dimension (d1 :* d2) where
  type DimFactorsOf (d1 :* d2) = (DimFactorsOf d1) @+ (DimFactorsOf d2)

instance (Unit u1, Unit u2) => Unit (u1 :* u2) where

  -- we override the default conversion lookup behavior
  type BaseUnit (u1 :* u2) = Canonical
  type DimOfUnit (u1 :* u2) = DimOfUnit u1 :* DimOfUnit u2
  conversionRatio _ = undefined -- this should never be called

  type UnitFactorsOf (u1 :* u2) = (UnitFactorsOf u1) @+ (UnitFactorsOf u2)
  canonicalConvRatio _ = canonicalConvRatio (undefined :: u1) *
                         canonicalConvRatio (undefined :: u2)

type instance DefaultUnitOfDim (d1 :* d2) =
  DefaultUnitOfDim d1 :* DefaultUnitOfDim d2

infixl 7 :/
-- | Divide two units to get another unit
data u1 :/ u2 = u1 :/ u2

instance (Dimension d1, Dimension d2) => Dimension (d1 :/ d2) where
  type DimFactorsOf (d1 :/ d2) = (DimFactorsOf d1) @- (DimFactorsOf d2)

instance (Unit u1, Unit u2) => Unit (u1 :/ u2) where
  type BaseUnit (u1 :/ u2) = Canonical
  type DimOfUnit (u1 :/ u2) = DimOfUnit u1 :/ DimOfUnit u2
  conversionRatio _ = undefined -- this should never be called
  type UnitFactorsOf (u1 :/ u2) = (UnitFactorsOf u1) @- (UnitFactorsOf u2)
  canonicalConvRatio _ = canonicalConvRatio (undefined :: u1) /
                         canonicalConvRatio (undefined :: u2)

type instance DefaultUnitOfDim (d1 :/ d2) =
  DefaultUnitOfDim d1 :/ DefaultUnitOfDim d2
  
infixr 8 :^
-- | Raise a unit to a power, known at compile time
data unit :^ (power :: Z) = unit :^ Sing power

instance Dimension dim => Dimension (dim :^ power) where
  type DimFactorsOf (dim :^ power) = (DimFactorsOf dim) @* power

instance (Unit unit, SingI power) => Unit (unit :^ power) where
  type BaseUnit (unit :^ power) = Canonical
  type DimOfUnit (unit :^ power) = DimOfUnit unit :^ power
  conversionRatio _ = undefined

  type UnitFactorsOf (unit :^ power) = (UnitFactorsOf unit) @* power
  canonicalConvRatio _ = canonicalConvRatio (undefined :: unit) ^^ (szToInt (sing :: Sing power))

type instance DefaultUnitOfDim (d :^ z) = DefaultUnitOfDim d :^ z

infixr 9 :@
-- | Multiply a conversion ratio by some constant. Used for defining prefixes.
data prefix :@ unit = prefix :@ unit

-- | A class for user-defined prefixes
class UnitPrefix prefix where
  -- | This should return the desired multiplier for the prefix being defined.
  -- This function must /not/ inspect its argument.
  multiplier :: Fractional f => prefix -> f

instance ( (unit == Canonical) ~ False
         , Unit unit
         , UnitPrefix prefix ) => Unit (prefix :@ unit) where
  type BaseUnit (prefix :@ unit) = unit
  conversionRatio _ = multiplier (undefined :: prefix)

