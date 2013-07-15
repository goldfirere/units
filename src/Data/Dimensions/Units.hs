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

-- | Dummy type use just to label canonical units. It does /not/ have a
-- 'Unit' instance.
data Canonical

-- | Class of units. Make an instance of this class to define a new unit.
class Unit unit where
  -- | The base unit of this unit: what this unit is defined in terms of.
  -- For units that are not defined in terms of anything else, the base unit
  -- should be 'Canonical'.
  type BaseUnit unit :: *

  -- | The conversion ratio /from/ the base unit /to/ this unit.
  -- If left out, a conversion ratio of 1 is assumed.
  --
  -- For example:
  --
  -- > instance Unit Foot where
  -- >   type BaseUnit Foot = Meter
  -- >   conversionRatio _ = 0.3048
  --
  -- Implementations should /never/ examine their argument!
  conversionRatio :: unit -> Double

  -- | The internal list of dimensions for a dimensioned quantity built from
  -- this unit.
  type DimSpecsOf unit :: [DimSpec *]
  type DimSpecsOf unit = If (IsCanonical unit)
                          '[D unit One]
                          (DimSpecsOf (BaseUnit unit))

  -- if unspecified, assume a conversion ratio of 1
  conversionRatio _ = 1

  -- | Compute the conversion from the underlying canonical unit to
  -- this one. A default is provided that multiplies together the ratios
  -- of all units between this one and the canonical one.
  canonicalConvRatio :: unit -> Double
  default canonicalConvRatio :: BaseHasConvRatio unit => unit -> Double
  canonicalConvRatio u = conversionRatio u * baseUnitRatio u

-- Abbreviation for creating a Dim (defined here to avoid a module cycle)
-- | Make a dimensioned quantity capable of storing a value of a given unit.
-- For example:
--
-- > type Length = MkDim Meter
type MkDim unit = Dim (DimSpecsOf unit)

-- | Is this unit a canonical unit?
type IsCanonical (unit :: *) = CheckCanonical (BaseUnit unit)

-- | Is the argument the special datatype 'Canonical'?
type family CheckCanonical (base_unit :: *) :: Bool where
  CheckCanonical Canonical = True
  CheckCanonical unit      = False

{- I want to say this. But type families are *eager* so I have to write
   it another way.
type family CanonicalUnit (unit :: *) where
  CanonicalUnit unit
    = If (IsCanonical unit) unit (CanonicalUnit (BaseUnit unit))
-}

-- | Get the canonical unit from a given unit. For example: @CanonicalUnit Foot = Meter@
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

  