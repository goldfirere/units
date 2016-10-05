{- Data/Metrology/Units.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file defines the class Unit, which is needed for
   user-defined units.
-}

{-# LANGUAGE TypeFamilies, DataKinds, DefaultSignatures, MultiParamTypeClasses,
             ConstraintKinds, UndecidableInstances, FlexibleContexts, CPP,
             FlexibleInstances, ScopedTypeVariables, TypeOperators, PolyKinds #-}

#if __GLASGOW_HASKELL__ >= 711
{-# LANGUAGE UndecidableSuperClasses #-}
#endif

module Data.Metrology.Units where

import Data.Metrology.Z
import Data.Metrology.Factor
import Data.Metrology.Dimensions
import Data.Metrology.LCSU
import Data.Type.Bool
import Data.Type.Equality
import Data.Proxy
import Data.Singletons
import GHC.Exts

-----------------------------------------------------------------------
-- Main Unit definitions (rather user-facing)
-----------------------------------------------------------------------

-- | Dummy type use just to label canonical units. It does /not/ have a
-- 'Unit' instance.
data Canonical

class DimOfUnitIsConsistent unit => Unit unit where
  -- | The base unit of this unit: what this unit is defined in terms of.
  -- For units that are not defined in terms of anything else, the base unit
  -- should be 'Canonical'.
  type BaseUnit unit :: *

  -- | The dimension that this unit is associated with. This needs to be
  -- defined only for canonical units; other units are necessarily of the
  -- same dimension as their base.
  type DimOfUnit unit :: *
  type DimOfUnit unit = DimOfUnit (BaseUnit unit)

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
  conversionRatio :: unit -> Rational
  conversionRatio _ = 1  -- if unspecified, assume a conversion ratio of 1

  -- | The internal list of canonical units corresponding to this unit.
  -- Overriding the default should not be necessary in user code.
  type UnitFactorsOf unit :: [Factor *]
  type UnitFactorsOf unit = If (IsCanonical unit)
                            '[F unit One]
                            (UnitFactorsOf (BaseUnit unit))

  -- | Compute the conversion from the underlying canonical unit to
  -- this one. A default is provided that multiplies together the ratios
  -- of all units between this one and the canonical one.
  canonicalConvRatio :: unit -> Rational
  default canonicalConvRatio :: BaseHasConvRatio unit => unit -> Rational
  canonicalConvRatio u = conversionRatio u * baseUnitRatio u

-- | Check to make sure that a unit has the same dimension as its base unit,
-- if any.
type family DimOfUnitIsConsistent unit :: Constraint where
  DimOfUnitIsConsistent unit = ( Dimension (DimOfUnit unit)
                               , If (BaseUnit unit == Canonical)
                                    (() :: Constraint)
                                    (DimOfUnit unit ~ DimOfUnit (BaseUnit unit)) )
  -- This definition does not use || so that we get better error messages.

-----------------------------------------------------------------------
-- Internal implementation details
-----------------------------------------------------------------------

-- | Is this unit a canonical unit?
type family IsCanonical (unit :: *) where
  IsCanonical unit = (BaseUnit unit == Canonical)
  -- this is a type family because of GHC #8978

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
type family BaseHasConvRatio unit where
  BaseHasConvRatio unit = HasConvRatio (IsCanonical unit) unit
  -- this is a type family because of GHC #8978

-- | This is like 'Unit', but deals with 'Canonical'. It is necessary
-- to be able to define 'canonicalConvRatio' in the right way.
class is_canonical ~ IsCanonical unit
      => HasConvRatio (is_canonical :: Bool) (unit :: *) where
  baseUnitRatio :: unit -> Rational
instance True ~ IsCanonical canonical_unit
         => HasConvRatio True canonical_unit where
  baseUnitRatio _ = 1
instance ( False ~ IsCanonical noncanonical_unit
         , Unit (BaseUnit noncanonical_unit) )
         => HasConvRatio False noncanonical_unit where
  baseUnitRatio _ = canonicalConvRatio (undefined :: BaseUnit noncanonical_unit)

-----------------------------------------------------------------------
-- Conversion ratios for lists of units
-----------------------------------------------------------------------

type family Units (dfactors :: [Factor *]) :: Constraint where
  Units '[]                    = ()
  Units (F unit z ': dfactors) = (Unit unit, Units dfactors)

-- | Classifies well-formed list of unit factors, and permits calculating a
-- conversion ratio for the purposes of LCSU conversions.
class (Units units) => UnitFactor (units :: [Factor *]) where
  canonicalConvRatioSpec :: Proxy units -> Rational

instance UnitFactor '[] where
  canonicalConvRatioSpec _ = 1

instance (UnitFactor rest, Unit unit, SingI n) => UnitFactor (F unit n ': rest) where
  canonicalConvRatioSpec _ =
    (canonicalConvRatio (undefined :: unit) ^^ szToInt (sing :: Sing n)) *
    canonicalConvRatioSpec (Proxy :: Proxy rest)

-------------------------------------------------------------
--- "Number" unit -------------------------------------------
-------------------------------------------------------------

-- | The dimension for the dimensionless quantities.
-- It is also called "quantities of dimension one", but
-- @One@ is confusing with the type-level integer One.
data Dimensionless = Dimensionless
instance Dimension Dimensionless where
  type DimFactorsOf Dimensionless = '[]
type instance DefaultUnitOfDim Dimensionless = Number

-- | The unit for unitless dimensioned quantities
data Number = Number -- the unit for unadorned numbers
instance Unit Number where
  type BaseUnit Number = Canonical
  type DimOfUnit Number = Dimensionless
  type UnitFactorsOf Number = '[]
