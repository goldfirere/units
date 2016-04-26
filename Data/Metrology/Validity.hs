{- Data/Metrology/Validity.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file defines validity checks on dimension, unit, and LCSU definitions.
-}

{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, PolyKinds,
             UndecidableInstances #-}

module Data.Metrology.Validity where

import Data.Metrology.LCSU
import Data.Metrology.Factor
import Data.Metrology.Dimensions
import Data.Metrology.Units
import Data.Metrology.Set
import Data.Metrology.Combinators
import GHC.Exts ( Constraint )

------------------------------------------------
-- Helper functions
------------------------------------------------

-- | Extract a dimension specifier from a list of factors
type family MultDimFactors (facts :: [Factor *]) where
  MultDimFactors '[] = Dimensionless
  MultDimFactors (F d z ': ds) = (d :^ z) :* MultDimFactors ds

-- | Extract a unit specifier from a list of factors
type family MultUnitFactors (facts :: [Factor *]) where
  MultUnitFactors '[] = Number
  MultUnitFactors (F unit z ': units) = (unit :^ z) :* MultUnitFactors units

-- | Extract a unit from a dimension factor list and an LCSU
type family UnitOfDimFactors (dims :: [Factor *]) (lcsu :: LCSU *) :: * where
  UnitOfDimFactors dims lcsu = MultUnitFactors (LookupList dims lcsu)

type family Units (dfactors :: [Factor *]) :: Constraint where
  Units '[]                    = ()
  Units (F unit z ': dfactors) = (Unit unit, Units dfactors)

------------------------------------------------
-- Main validity functions
------------------------------------------------

-- | Check if a (dimension factors, LCSU, unit) triple are all valid to be used together.
type family ValidDLU (dfactors :: [Factor *]) (lcsu :: LCSU *) (unit :: *) where
  ValidDLU dfactors lcsu unit =
    ( dfactors ~ DimFactorsOf (DimOfUnit unit)
    , UnitFactor (LookupList dfactors lcsu)
    , Units (LookupList dfactors lcsu)  -- needed only in GHC 8
    , Unit unit
    , UnitFactorsOf unit *~ LookupList dfactors lcsu )

-- | Check if a (dimension factors, LCSU) pair are valid to be used together. This
-- checks that each dimension maps to a unit of the correct dimension.
type family ValidDL (dfactors :: [Factor *]) (lcsu :: LCSU *) :: Constraint where
  ValidDL dfactors lcsu = ValidDLU dfactors lcsu (UnitOfDimFactors dfactors lcsu)

-- | Are two LCSUs inter-convertible at the given dimension?
type family ConvertibleLCSUs (dfactors :: [Factor *])
                             (l1 :: LCSU *) (l2 :: LCSU *) :: Constraint where
  ConvertibleLCSUs dfactors l1 l2 =
    ( LookupList dfactors l1 *~ LookupList dfactors l2
    , ValidDL dfactors l1
    , ValidDL dfactors l2
    , UnitFactor (LookupList dfactors l1)
    , UnitFactor (LookupList dfactors l2) )

-- | Like 'ConvertibleLCSUs', but takes a dimension, not a dimension factors.
type family ConvertibleLCSUs_D (dim :: *) (l1 :: LCSU *) (l2 :: LCSU *) :: Constraint where
  ConvertibleLCSUs_D dim l1 l2 = ConvertibleLCSUs (DimFactorsOf dim) l1 l2

infix 4 *~
-- | Check if two @[Factor *]@s, representing /units/, should be
-- considered to be equal
type family (units1 :: [Factor *]) *~ (units2 :: [Factor *]) :: Constraint where
  units1 *~ units2 =
    CanonicalUnitsOfFactors units1 `SetEqual` CanonicalUnitsOfFactors units2

-- | Given a list of unit factors, extract out the canonical units they are based
-- on.
type family CanonicalUnitsOfFactors (fs :: [Factor *]) :: [*] where
  CanonicalUnitsOfFactors '[] = '[]
  CanonicalUnitsOfFactors (F u z ': fs) = (CanonicalUnit u) ': CanonicalUnitsOfFactors fs

-- | Check if an LCSU has consistent entries for the given unit. i.e. can the lcsu
--   describe the unit?
type family CompatibleUnit (lcsu :: LCSU *) (unit :: *) :: Constraint where
  CompatibleUnit lcsu unit
   = ( ValidDLU (DimFactorsOf (DimOfUnit unit)) lcsu unit
     , UnitFactor (LookupList (DimFactorsOf (DimOfUnit unit)) lcsu) )

-- | Check if an LCSU can express the given dimension
type family CompatibleDim (lcsu :: LCSU *) (dim :: *) :: Constraint where
  CompatibleDim lcsu dim
    = ( UnitFactor (LookupList (DimFactorsOf dim) lcsu)
      , DimOfUnit (Lookup dim lcsu) ~ dim )

-- | Check if the 'DefaultLCSU' can convert into the given one, at the given
-- dimension.
type family DefaultConvertibleLCSU_D (dim :: *) (l :: LCSU *) :: Constraint where
  DefaultConvertibleLCSU_D dim l =
    ( ValidDL (DimFactorsOf dim) DefaultLCSU
    , ConvertibleLCSUs (DimFactorsOf dim) DefaultLCSU l )

-- | Check if the 'DefaultLCSU' can convert into the given one, at the given
-- unit.
type family DefaultConvertibleLCSU_U (unit :: *) (l :: LCSU *) :: Constraint where
  DefaultConvertibleLCSU_U unit l =
    DefaultConvertibleLCSU_D (DimOfUnit unit) l
