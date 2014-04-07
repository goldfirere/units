{- Data/Dimensions/Units.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file defines the class Unit, which is needed for
   user-defined units.
-}

{-# LANGUAGE TypeFamilies, DataKinds, DefaultSignatures, MultiParamTypeClasses,
             ConstraintKinds, UndecidableInstances, FlexibleContexts,
             FlexibleInstances, ScopedTypeVariables, TypeOperators #-}

module Data.Dimensions.Units where

import Data.Dimensions.Z
import Data.Dimensions.DimSpec
import Data.Dimensions.Dim
import Data.Dimensions.Map
import Data.Type.Bool
import Data.Proxy
import Data.Singletons

-- | Dummy type use just to label canonical units. It does /not/ have a
-- 'Unit' instance.
data Canonical

-- | TODO
class Dimension dim where
  -- | TODO
  type DimSpecsOf dim :: [DimSpec *]
  type DimSpecsOf dim = '[D dim One]
  
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
  conversionRatio :: Fractional f => unit -> f
  conversionRatio _ = 1  -- if unspecified, assume a conversion ratio of 1

  -- | The internal list of canonical units corresponding to this unit.
  type UnitSpecsOf unit :: [DimSpec *]
  type UnitSpecsOf unit = If (IsCanonical unit)
                            '[D unit One]
                            (UnitSpecsOf (BaseUnit unit))

  -- | Compute the conversion from the underlying canonical unit to
  -- this one. A default is provided that multiplies together the ratios
  -- of all units between this one and the canonical one.
  canonicalConvRatio :: Fractional f => unit -> f
  default canonicalConvRatio :: (BaseHasConvRatio unit, Fractional f)
                             => unit -> f
  canonicalConvRatio u = conversionRatio u * baseUnitRatio u

-- Abbreviation for creating a Dim (defined here to avoid a module cycle)

-- | Make a dimensioned quantity type capable of storing a value of a given
-- unit. This uses a 'Double' for storage of the value. For example:
--
-- > type Length = MkDim Meter
type MkDim dim lcsu = Dim Double (DimSpecsOf dim) lcsu

-- | Make a dimensioned quantity with a custom numerical type.
type MkGenDim n dim lcsu = Dim n (DimSpecsOf dim) lcsu

-- extracting a dimension from a unit
type family DimSpecsOfUnit (unit :: *) (lcsu :: Map *) :: [DimSpec *] where
  DimSpecsOfUnit unit lcsu = LookupList (UnitSpecsOf unit) (RevMap lcsu)

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

-- | Get the canonical unit from a given unit.
-- For example: @CanonicalUnit Foot = Meter@
type CanonicalUnit (unit :: *) = CanonicalUnit' (BaseUnit unit) unit

-- | Helper function in 'CanonicalUnit'
type family CanonicalUnit' (base_unit :: *) (unit :: *) :: * where
  CanonicalUnit' Canonical unit = unit
  CanonicalUnit' base      unit = CanonicalUnit' (BaseUnit base) base

-- | Essentially, a constraint that checks if a conversion ratio can be
-- calculated for a @BaseUnit@ of a unit.
type BaseHasConvRatio unit = HasConvRatio (IsCanonical unit) unit

-- | This is like 'Unit', but deals with 'Canonical'. It is necessary
-- to be able to define 'canonicalConvRatio' in the right way.
class is_canonical ~ IsCanonical unit
      => HasConvRatio (is_canonical :: Bool) (unit :: *) where
  baseUnitRatio :: Fractional f => unit -> f
instance True ~ IsCanonical canonical_unit
         => HasConvRatio True canonical_unit where
  baseUnitRatio _ = 1
instance ( False ~ IsCanonical noncanonical_unit
         , Unit (BaseUnit noncanonical_unit) )
         => HasConvRatio False noncanonical_unit where
  baseUnitRatio _ = canonicalConvRatio (undefined :: BaseUnit noncanonical_unit)

class UnitSpec (units :: [DimSpec *]) where
  canonicalConvRatioSpec :: Fractional f => Proxy units -> f

instance UnitSpec '[] where
  canonicalConvRatioSpec _ = 1

instance (UnitSpec rest, Unit unit, SingI n) => UnitSpec (D unit n ': rest) where
  canonicalConvRatioSpec _ =
    (canonicalConvRatio (undefined :: unit) ^^ szToInt (sing :: Sing n)) *
    canonicalConvRatioSpec (Proxy :: Proxy rest)

infix 4 *~
-- | Check if two @[DimSpec *]@s, representing /units/, should be
-- considered to be equal
type units1 *~ units2 = (Canonicalize units1 @~ Canonicalize units2)

type family Canonicalize (units :: [DimSpec *]) :: [DimSpec *] where
  Canonicalize '[] = '[]
  Canonicalize (D unit n ': rest) = D (CanonicalUnit unit) n ': Canonicalize rest

type Compatible (dim :: *) (lcsu :: Map *) (unit :: *) =
  ( CanonicalUnit (Lookup dim lcsu) ~ CanonicalUnit unit
  , Unit (Lookup dim lcsu))
