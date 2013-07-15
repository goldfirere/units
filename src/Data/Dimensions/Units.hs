{- Data/Dimensions/Units.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file defines the class Unit, which is needed for
   user-defined units.
-}

{-# LANGUAGE TypeFamilies, DataKinds, DefaultSignatures, MultiParamTypeClasses,
             ConstraintKinds, UndecidableInstances, FlexibleContexts,
             FlexibleInstances, ScopedTypeVariables #-}

module Data.Dimensions.Units where

import Data.Dimensions.Z
import Data.Dimensions.DimSpec
import Data.Dimensions.Dim
import Data.Dimensions.TypePrelude

data Canonical -- marker of a canonical unit

class Unit unit where
  type BaseUnit unit :: *
  conversionRatio :: unit -> Double

  type DimSpecsOf unit :: [DimSpec *]
  type DimSpecsOf unit = If (IsCanonical unit)
                          '[D unit One]
                          (DimSpecsOf (BaseUnit unit))

  -- if unspecified, assume a conversion ratio of 1
  conversionRatio _ = 1

  canonicalConvRatio :: unit -> Double
  default canonicalConvRatio :: BaseHasConvRatio unit => unit -> Double
  canonicalConvRatio u = conversionRatio u * baseUnitRatio u

-- Abbreviation for creating a Dim (defined here to avoid a module cycle)
type MkDim unit = Dim (DimSpecsOf unit)

type IsCanonical (unit :: *) = CheckCanonical (BaseUnit unit)
type family CheckCanonical (base_unit :: *) :: Bool where
  CheckCanonical Canonical = True
  CheckCanonical unit      = False

{- I want to say this. But type families are *eager* so I have to write
   it another way.
type family CanonicalUnit (unit :: *) where
  CanonicalUnit unit
    = If (IsCanonical unit) unit (CanonicalUnit (BaseUnit unit))
-}

type CanonicalUnit (unit :: *) = CanonicalUnit' (BaseUnit unit) unit
type family CanonicalUnit' (base_unit :: *) (unit :: *) :: * where
  CanonicalUnit' Canonical unit = unit
  CanonicalUnit' base      unit = CanonicalUnit' (BaseUnit base) base

type BaseHasConvRatio unit = HasConvRatio (IsCanonical unit) unit

class is_canonical ~ IsCanonical unit
      => HasConvRatio (is_canonical :: Bool) (unit :: *) where
  baseUnitRatio :: unit -> Double
instance True ~ IsCanonical canonical_unit
         => HasConvRatio True canonical_unit where
  baseUnitRatio _ = 1
instance ( False ~ IsCanonical noncanonical_unit
         , Unit (BaseUnit noncanonical_unit) )
         => HasConvRatio False noncanonical_unit where
  baseUnitRatio _ = canonicalConvRatio (undefined :: BaseUnit noncanonical_unit)

  