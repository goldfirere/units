{- Data/Metrology/Poly.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file gathers and exports all user-visible pieces of the units package.
   It also defines the main creators and consumers of dimensioned objects.
-}

{-# LANGUAGE ExplicitNamespaces, DataKinds, FlexibleInstances, TypeFamilies,
             TypeOperators, ConstraintKinds, ScopedTypeVariables,
             FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Poly
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports all the gubbins needed for type-checking your
-- dimensioned quantities. See 'Data.Metrology' for some functions
-- restricted to using a default LCSU, which is suitable for many
-- applications.
-----------------------------------------------------------------------------

module Data.Metrology.Poly (
  -- * Term-level combinators

  -- | The term-level arithmetic operators are defined by
  -- applying vertical bar(s) to the sides the dimensioned 
  -- quantities acts on.

  -- ** Additive operations
  zero, (|+|), (|-|), qSum, qNegate,

  -- ** Multiplicative operations between non-vector quantities
  (|*|), (|/|), (/|),

  -- ** Multiplicative operations between a vector and a scalar
  (*|), (|*), (|/),

  -- ** Exponentiation
  (|^), (|^^), qNthRoot,
  qSq, qCube, qSqrt, qCubeRoot,

  -- ** Comparison
  qCompare, (|<|), (|>|), (|<=|), (|>=|), (|==|), (|/=|),
  qApprox, qNapprox,        

  -- * Nondimensional units, conversion between quantities and numeric values
  unity, redim, convert,
  numIn, (#), quOf, (%), defaultLCSU, fromDefaultLCSU,
  constant,

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

  -- * Numbers, the only built-in unit
  Dimensionless(..), Number(..), Count, quantity,

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
  sZero, sOne, sTwo, sThree, sFour, sFive,
  sMOne, sMTwo, sMThree, sMFour, sMFive,
  sSucc, sPred, sNegate,

  -- ** Deprecated synonyms for the ones above
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

import Data.VectorSpace

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
         , VectorSpace n
         , Fractional (Scalar n) )
      => Qu dim lcsu n -> unit -> n
numIn (Qu val) u
  = val ^* fromRational
             (canonicalConvRatioSpec (Proxy :: Proxy (LookupList dim lcsu))
              / canonicalConvRatio u)

infix 5 #
-- | Infix synonym for 'numIn'
(#) :: ( ValidDLU dim lcsu unit
       , VectorSpace n
       , Fractional (Scalar n) )
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
         , VectorSpace n
         , Fractional (Scalar n) )
      => n -> unit -> Qu dim lcsu n
quOf d u
  = Qu (d ^* fromRational
               (canonicalConvRatio u
                / canonicalConvRatioSpec (Proxy :: Proxy (LookupList dim lcsu))))

infixr 9 %
-- | Infix synonym for 'quOf'
(%) :: ( ValidDLU dim lcsu unit
       , VectorSpace n
       , Fractional (Scalar n) )
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


-- | Compute the argument in the DefaultLCSU, and present the result
-- as lcsu-polymorphic dimension-polymorphic value.
fromDefaultLCSU :: ( d @~ e
                   , ConvertibleLCSUs e DefaultLCSU l
                   , Fractional n )
         => Qu d DefaultLCSU n -> Qu e l n
fromDefaultLCSU = convert . redim


-- | A synonym of 'fromDefaultLCSU', for one of its dominant usecase
-- is to inject constant quantities into dimension-polymorphic
-- expressions.
constant :: ( d @~ e
            , ConvertibleLCSUs e DefaultLCSU l
            , Fractional n )
         => Qu d DefaultLCSU n -> Qu e l n
constant = fromDefaultLCSU

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
-- This is parameterized by an LCSU and a number representation.
type Count = MkQu_ULN Number
