{- Data/Dimensions/UnitCombinators.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file defines combinators to build more complex units from simpler ones.
-}

{-# LANGUAGE TypeOperators, TypeFamilies, UndecidableInstances,
             ScopedTypeVariables, DataKinds, FlexibleInstances #-}

module Data.Dimensions.UnitCombinators where

import Data.Singletons ( Sing, SingI, sing )

import Data.Dimensions.Units
import Data.Dimensions.DimSpec
import Data.Dimensions.Z
import Data.Type.Bool

infixl 7 :*
-- | Multiply two units to get another unit.
-- For example: @type MetersSquared = Meter :* Meter@
data u1 :* u2 = u1 :* u2

instance (Unit u1, Unit u2) => Unit (u1 :* u2) where

  -- we override the default conversion lookup behavior
  type BaseUnit (u1 :* u2) = Canonical
  conversionRatio _ = undefined -- this should never be called

  type DimSpecsOf (u1 :* u2) = (DimSpecsOf u1) @+ (DimSpecsOf u2)
  canonicalConvRatio _ = canonicalConvRatio (undefined :: u1) *
                         canonicalConvRatio (undefined :: u2)

infixl 7 :/
-- | Divide two units to get another unit
data u1 :/ u2 = u1 :/ u2

instance (Unit u1, Unit u2) => Unit (u1 :/ u2) where
  type BaseUnit (u1 :/ u2) = Canonical
  conversionRatio _ = undefined -- this should never be called
  type DimSpecsOf (u1 :/ u2) = (DimSpecsOf u1) @- (DimSpecsOf u2)
  canonicalConvRatio _ = canonicalConvRatio (undefined :: u1) /
                         canonicalConvRatio (undefined :: u2)

infixr 8 :^
-- | Raise a unit to a power, known at compile time
data unit :^ (power :: Z) = unit :^ Sing power

instance (Unit unit, SingI power) => Unit (unit :^ power) where
  type BaseUnit (unit :^ power) = Canonical
  conversionRatio _ = undefined

  type DimSpecsOf (unit :^ power) = (DimSpecsOf unit) @* power
  canonicalConvRatio _ = canonicalConvRatio (undefined :: unit) ^^ (szToInt (sing :: Sing power))

infix 9 :@
-- | Multiply a conversion ratio by some constant. Used for defining prefixes.
data prefix :@ unit = prefix :@ unit

-- | A class for user-defined prefixes
class UnitPrefix prefix where
  -- | This should return the desired multiplier for the prefix being defined.
  -- This function must /not/ inspect its argument.
  multiplier :: prefix -> Double

instance ( CheckCanonical unit ~ False
         , Unit unit
         , UnitPrefix prefix ) => Unit (prefix :@ unit) where
  type BaseUnit (prefix :@ unit) = unit
  conversionRatio _ = multiplier (undefined :: prefix)
