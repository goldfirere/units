{- Data/Metrology.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file gathers and exports all user-visible pieces of the units package.
   It also defines the main creators and consumers of dimensioned objects.

   This package declares many closely-related types. The following naming
   conventions should be helpful:

   Prefix  Target type/kind
   ------------------------
     #     Z
     $     Factor *
     @     [Factor *]
     @@    [Factor *], where the arguments are ordered similarly
     %     Qu (at the type level)
     |     Qu (at the term level)
     :     units & dimensions, at both type and term levels
-}

{-# LANGUAGE ExplicitNamespaces, DataKinds, FlexibleInstances, TypeFamilies,
             TypeOperators, ConstraintKinds, ScopedTypeVariables,
             FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The units package is a framework for strongly-typed dimensional analysis.
-- This haddock documentation is generally /not/ enough to be able to use this
-- package effectively. Please see the readme at
-- <http://www.cis.upenn.edu/~eir/packages/units/README.html>.
--
-- Some of the types below refer to declarations that are not exported and
-- not documented here. This is because Haddock does not allow finely-tuned
-- abstraction in documentation. (In particular, right-hand sides of type 
-- synonym declarations are always included.) If a symbol is not exported,
-- you do /not/ need to know anything about it to use this package.
--
-- Though it doesn't appear here, @Scalar@ is an instance of @Num@, and
-- generally has all the numeric instances that @Double@ has.
-----------------------------------------------------------------------------

module Data.Metrology (
  -- * Term-level combinators

  -- | The term-level arithmetic operators are defined by
  -- applying vertical bar(s) to the sides the dimensioned 
  -- quantities acts on. 
  -- See also "Data.Metrology.AltOperators" for an alternative system of operators.
  (|+|), (|-|), 
  (|*|), (|/|), (*|),  (|*), (/|), (|/), 
  (|^), (|^^),
  (|<|), (|>|), (|<=|), (|>=|), (|==|), (|/=|),
  qApprox, qNapprox,        
  qSq, qCube, qSqrt, qCubeRoot, nthRoot, 

  -- * Nondimensional units, conversion between quantities and numeric values
  unity, zero, redim, convert, constant,
  numIn, (#), quOf, (%), defaultLCSU, -- fromDefaultLCSU,

  -- * Type-level unit combinators
  (:*)(..), (:/)(..), (:^)(..), (:@)(..),
  UnitPrefix(..),

  -- * Type-level quantity combinators
  type (%*), type (%/), type (%^),

  -- * Creating quantity types
  Qu, MkQu_D, MkQu_DLN, MkQu_U, MkQu_ULN, 

  -- * Creating new dimensions
  Dimension,

  -- * Creating new units
  Unit(type BaseUnit, type DimOfUnit, conversionRatio), 
  Canonical,

  -- * Scalars, the only built-in unit
  Dimensionless(..), Number(..), Scalar, scalar,

  -- * LCSUs (locally coherent system of units)
  MkLCSU, LCSU(DefaultLCSU), DefaultUnitOfDim,

  -- * Validity checks and assertions
  CompatibleUnit, CompatibleDim, ConvertibleLCSUs_D,
  DefaultConvertibleLCSU_D, DefaultConvertibleLCSU_U,

  -- * Type-level integers
  Z(..), Succ, Pred, type (#+), type (#-), type (#*), type (#/), NegZ,

  -- ** Synonyms for small numbers
  One, Two, Three, Four, Five, MOne, MTwo, MThree, MFour, MFive,

  -- ** Term-level singletons
  pZero, pOne, pTwo, pThree, pFour, pFive,
  pMOne, pMTwo, pMThree, pMFour, pMFive,
  pSucc, pPred,

  -- * Internal definitions
  -- | The following module is re-exported solely to prevent noise in error messages;
  -- we do not recommend trying to use these definitions in user code.
  module Data.Metrology.Internal

  ) where

import Data.Metrology.Z
import Data.Metrology.Quantity
import Data.Metrology.Dimensions
import Data.Metrology.Factor
import Data.Metrology.Units
import Data.Metrology.Combinators
import Data.Metrology.LCSU
import Data.Metrology.Validity
import Data.Metrology.Internal
import Data.Proxy

-- | Extracts a numerical value from a dimensioned quantity, expressed in
--   the given unit. For example:
--
--   > inMeters :: Length -> Double
--   > inMeters x = numIn x Meter
--
--   or
--
--   > inMeters x = x # Meter   
numIn :: forall unit dim lcsu n.
         ( ValidDLU dim lcsu unit
         , Fractional n )
      => Qu dim lcsu n -> unit -> n
numIn (Qu val) u
  = val * fromRational
            (canonicalConvRatioSpec (Proxy :: Proxy (LookupList dim lcsu))
             / canonicalConvRatio u)

infix 5 #
-- | Infix synonym for 'numIn'
(#) :: ( ValidDLU dim lcsu unit
       , Fractional n )
    => Qu dim lcsu n -> unit -> n
(#) = numIn

-- | Creates a dimensioned quantity in the given unit. For example:
--
--   > height :: Length
--   > height = quOf 2.0 Meter
--
--   or
--
--   > height = 2.0 % Meter
quOf :: forall unit dim lcsu n.
         ( ValidDLU dim lcsu unit
         , Fractional n )
      => n -> unit -> Qu dim lcsu n
quOf d u
  = Qu (d * fromRational
               (canonicalConvRatio u
                / canonicalConvRatioSpec (Proxy :: Proxy (LookupList dim lcsu))))

infixr 9 %
-- | Infix synonym for 'quOf'
(%) :: ( ValidDLU dim lcsu unit
       , Fractional n )
    => n -> unit -> Qu dim lcsu n
(%) = quOf

-- | Use this to choose a default LCSU for a dimensioned quantity.
-- The default LCSU uses the 'DefaultUnitOfDim' representation for each
-- dimension.
defaultLCSU :: Qu dim DefaultLCSU n -> Qu dim DefaultLCSU n
defaultLCSU = id

-- | The number 1, expressed as a unitless dimensioned quantity.
unity :: Num n => Qu '[] l n
unity = Qu 1

-- | The number 0, polymorphic in its dimension. Use of this will
-- often require a type annotation.
zero :: Num n => Qu dimspec l n
zero = Qu 0

-- | Cast between equivalent dimension within the same CSU.
--  for example [kg m s] and [s m kg]. See the README for more info.
redim :: (d @~ e) => Qu d l n -> Qu e l n
redim (Qu x) = Qu x

-- | Dimension-keeping cast between different CSUs.
convert :: forall d l1 l2 n. 
  ( ConvertibleLCSUs d l1 l2
  , Fractional n ) 
  => Qu d l1 n -> Qu d l2 n
convert (Qu x) = Qu $ x * fromRational (
  canonicalConvRatioSpec (Proxy :: Proxy (LookupList d l1))
  / canonicalConvRatioSpec (Proxy :: Proxy (LookupList d l2)))

{-
-- | Compute the argument in the DefaultLCSU, and present the result
-- as lcsu-polymorphic dimension-polymorphic value.
fromDefaultLCSU :: ( d @~ e
                   , UnitFactor (LookupList e l)
                   , LookupList d DefaultLCSU *~ LookupList e l
                   , Fractional n )
         => Qu d DefaultLCSU n -> Qu e l n
fromDefaultLCSU = convert . redim
-}

-- | A synonym of 'fromDefaultLCSU', for one of its dominant usecase
-- is to inject constant quantities into dimension-polymorphic
-- expressions.
constant :: ( ConvertibleLCSUs d DefaultLCSU l
            , Fractional n )
         => Qu d DefaultLCSU n -> Qu d l n
constant = convert

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

-- | The type of unitless dimensioned quantities.
-- This is an instance of @Num@, though Haddock doesn't show it.
-- This uses a @Double@ internally and uses a default LCSU.
type Scalar = MkQu_U Number

-- | Convert a raw number into a unitless dimensioned quantity
scalar :: n -> Qu '[] l n
scalar = Qu
