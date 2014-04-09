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
import Data.Dimensions.LCSU
import Data.Type.Bool
import Data.Type.Equality
import Data.Proxy
import Data.Singletons
import GHC.Exts

-- | Dummy type use just to label canonical units. It does /not/ have a
-- 'Unit' instance.
data Canonical

-- | This class is used to mark abstract dimensions, such as @Length@, or
-- @Mass@.
class Dimension dim where
  -- | Retrieve a list of @DimSpec@s representing the given dimension. Overriding
  -- the default of this type family should not be necessary in user code.
  type DimSpecsOf dim :: [DimSpec *]
  type DimSpecsOf dim = '[D dim One]
  
-- | Class of units. Make an instance of this class to define a new unit.
-- A minimal complete definition is either
--
-- @
-- instance Unit Foo where
--   type BaseUnit Foo = Canonical
--   type DimOfUnit Foo = Bar     -- where we have instance Dimension Bar
-- @
--
-- OR
--
-- @
-- instance Unit Foo where
--   type BaseUnit Foo = Baz
--   conversionRatio _ = 3.14   -- here, Foo is a /bigger/ unit than Baz
-- @
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
  type UnitSpecsOf unit :: [DimSpec *]
  type UnitSpecsOf unit = If (IsCanonical unit)
                            '[D unit One]
                            (UnitSpecsOf (BaseUnit unit))

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

-- Abbreviation for creating a Dim (defined here to avoid a module cycle)

-- | Make a dimensioned quantity type capable of storing a value of a given
-- unit. This uses a 'Double' for storage of the value. For example:
--
-- > data LengthDim = LengthDim
-- > instance Dimension LengthDim
-- > type Length = MkDim LengthDim
type MkDim dim = Dim (DimSpecsOf dim) DefaultLCSU Double

-- | Make a dimensioned quantity with a custom numerical type and LCSU.
type MkGenDim dim lcsu n = Dim (DimSpecsOf dim) lcsu n

-- | Is this unit a canonical unit?
type IsCanonical (unit :: *) = (BaseUnit unit == Canonical)

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
  baseUnitRatio :: unit -> Rational
instance True ~ IsCanonical canonical_unit
         => HasConvRatio True canonical_unit where
  baseUnitRatio _ = 1
instance ( False ~ IsCanonical noncanonical_unit
         , Unit (BaseUnit noncanonical_unit) )
         => HasConvRatio False noncanonical_unit where
  baseUnitRatio _ = canonicalConvRatio (undefined :: BaseUnit noncanonical_unit)

class UnitSpec (units :: [DimSpec *]) where
  canonicalConvRatioSpec :: Proxy units -> Rational

instance UnitSpec '[] where
  canonicalConvRatioSpec _ = 1

-- the instances for S n and P n must be separate to allow for the Zero case,
-- which comes up for a DefaultLCSU
instance (UnitSpec rest, Unit unit, SingI n) => UnitSpec (D unit (S n) ': rest) where
  canonicalConvRatioSpec _ =
    (canonicalConvRatio (undefined :: unit) ^^ szToInt (sing :: Sing (S n))) *
    canonicalConvRatioSpec (Proxy :: Proxy rest)

instance (UnitSpec rest, Unit unit, SingI n) => UnitSpec (D unit (P n) ': rest) where
  canonicalConvRatioSpec _ =
    (canonicalConvRatio (undefined :: unit) ^^ szToInt (sing :: Sing (P n))) *
    canonicalConvRatioSpec (Proxy :: Proxy rest)

instance UnitSpec '[D DefaultLCSUUnit Zero] where
  canonicalConvRatioSpec _ = 1
                                                          
infix 4 *~
-- | Check if two @[DimSpec *]@s, representing /units/, should be
-- considered to be equal
type family units1 *~ units2 where
  '[D DefaultLCSUUnit Zero] *~ units2 = (() :: Constraint)
  units1 *~ '[D DefaultLCSUUnit Zero] = (() :: Constraint)
  units1 *~ units2 = (Canonicalize units1 @~ Canonicalize units2)

-- | Given a unit specification, get the canonical units of each component.
type family Canonicalize (units :: [DimSpec *]) :: [DimSpec *] where
  Canonicalize '[] = '[]
  Canonicalize (D unit n ': rest) = D (CanonicalUnit unit) n ': Canonicalize rest

-- | Check if an LCSU has consistent entries for the given unit.
type family Compatible (lcsu :: LCSU *) (unit :: *) :: Constraint where
  Compatible lcsu unit
   = ( UnitSpecsOf unit *~ LookupList (DimSpecsOf (DimOfUnit unit)) lcsu
     , UnitSpec (LookupList (DimSpecsOf (DimOfUnit unit)) lcsu) )
