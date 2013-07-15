{- Data/Dimensions/UnitCombinators.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file defines combinators to build more complex units from simpler ones.
-}

{-# LANGUAGE TypeOperators, TypeFamilies, UndecidableInstances,
             ScopedTypeVariables, DataKinds, FlexibleInstances #-}

module Data.Dimensions.UnitCombinators where

import GHC.TypeLits ( Sing, SingI, sing )

import Data.Dimensions.Units
import Data.Dimensions.DimSpec
import Data.Dimensions.Z

-- multiplication
infixl 7 :*
data u1 :* u2 = u1 :* u2

instance (Unit u1, Unit u2) => Unit (u1 :* u2) where

  -- we override the default conversion lookup behavior
  type BaseUnit (u1 :* u2) = Canonical
  conversionRatio _ = undefined -- this should never be called

  type DimSpecsOf (u1 :* u2) = (DimSpecsOf u1) @+ (DimSpecsOf u2)
  canonicalConvRatio _ = canonicalConvRatio (undefined :: u1) *
                         canonicalConvRatio (undefined :: u2)

-- division
infixl 7 :/
data u1 :/ u2 = u1 :/ u2

instance (Unit u1, Unit u2) => Unit (u1 :/ u2) where
  type BaseUnit (u1 :/ u2) = Canonical
  conversionRatio _ = undefined -- this should never be called
  type DimSpecsOf (u1 :/ u2) = (DimSpecsOf u1) @- (DimSpecsOf u2)
  canonicalConvRatio _ = canonicalConvRatio (undefined :: u1) /
                         canonicalConvRatio (undefined :: u2)

-- power
infixr 8 :^
data unit :^ (power :: Z) = unit :^ Sing power

instance (Unit unit, SingI power) => Unit (unit :^ power) where
  type BaseUnit (unit :^ power) = Canonical
  conversionRatio _ = undefined

  type DimSpecsOf (unit :^ power) = (DimSpecsOf unit) @* power
  canonicalConvRatio _ = canonicalConvRatio (undefined :: unit) ^^ (szToInt (sing :: Sing power))

-- prefixes
infix 9 :@
data prefix :@ unit = prefix :@ unit
class UnitPrefix prefix where
  multiplier :: prefix -> Double

instance ( CheckCanonical unit ~ False
         , Unit unit
         , UnitPrefix prefix ) => Unit (prefix :@ unit) where
  type BaseUnit (prefix :@ unit) = unit
  conversionRatio _ = multiplier (undefined :: prefix)