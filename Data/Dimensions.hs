{- Data/Dimensions.hs

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
     $     DimSpec *
     @     [DimSpec *]
     @@    [DimSpec *], where the arguments are ordered similarly
     %     Dim (at the type level)
     .     Dim (at the term level)
     :     units, at both type and term levels
-}

{-# LANGUAGE ExplicitNamespaces, DataKinds, FlexibleInstances, TypeFamilies,
             TypeOperators, ConstraintKinds, ScopedTypeVariables,
             FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dimensions
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

module Data.Dimensions (
  -- * Term-level combinators
  (.+), (.-), (.*), (./), (.^), (*.),
  (.<), (.>), (.<=), (.>=), dimEq, dimNeq,
  nthRoot, dimSqrt, dimCubeRoot,
  unity, zero, dim,
  dimIn, (#), dimOf, (%), defaultLCSU,

  -- * Type-level unit combinators
  (:*)(..), (:/)(..), (:^)(..), (:@)(..),
  UnitPrefix(..),

  -- * Type-level dimensioned-quantity combinators
  type (%*), type (%/), type (%^),

  -- * Creating new dimensions
  Dimension, MkLCSU, LCSU(DefaultLCSU),

  -- * Creating new units
  Unit(type BaseUnit, type DimOfUnit, conversionRatio), MkDim, MkGenDim, 
  Canonical,

  -- * Scalars, the only built-in unit
  Dimensionless(..), Number(..), Scalar, scalar,

  -- * Type-level integers
  Z(..), Succ, Pred, type (#+), type (#-), type (#*), type (#/), NegZ,

  -- ** Synonyms for small numbers
  One, Two, Three, Four, Five, MOne, MTwo, MThree, MFour, MFive,

  -- ** Term-level singletons
  pZero, pOne, pTwo, pThree, pFour, pFive,
  pMOne, pMTwo, pMThree, pMFour, pMFive,
  pSucc, pPred

  ) where

import Data.Dimensions.Z
import Data.Dimensions.Dim
import Data.Dimensions.DimSpec
import Data.Dimensions.Units
import Data.Dimensions.UnitCombinators
import Data.Dimensions.Map
import Data.Proxy

-- | Extracts a @Double@ from a dimensioned quantity, expressed in
--   the given unit. For example:
--
--   > inMeters :: Length -> Double
--   > inMeters x = dimIn x Meter
dimIn :: forall unit dim lcsu n.
         ( Unit unit
         , UnitSpec (LookupList dim lcsu)
         , UnitSpecsOf unit *~ LookupList dim lcsu
         , Fractional n )
      => Dim n dim lcsu -> unit -> n
dimIn (Dim val) u = val * canonicalConvRatioSpec (Proxy :: Proxy (LookupList dim lcsu))
                        / canonicalConvRatio u

infix 5 #
-- | Infix synonym for 'dimIn'
(#) :: ( Unit unit
       , UnitSpec (LookupList dim lcsu)
       , UnitSpecsOf unit *~ LookupList dim lcsu
       , Fractional n )
    => Dim n dim lcsu -> unit -> n
(#) = dimIn

-- | Creates a dimensioned quantity in the given unit. For example:
--
--   > height :: Length
--   > height = dimOf 2.0 Meter
dimOf :: forall unit dim lcsu n.
         ( dim ~ DimSpecsOf (DimOfUnit unit)
         , Unit unit
         , UnitSpec (LookupList dim lcsu)
         , UnitSpecsOf unit *~ LookupList dim lcsu
         , Fractional n )
      => n -> unit -> Dim n dim lcsu
dimOf d u = Dim (d * canonicalConvRatio u
                   / canonicalConvRatioSpec (Proxy :: Proxy (LookupList dim lcsu)))

infixr 9 %
-- | Infix synonym for 'dimOf'
(%) :: ( dim ~ DimSpecsOf (DimOfUnit unit)
       , Unit unit
       , UnitSpec (LookupList dim lcsu)
       , UnitSpecsOf unit *~ LookupList dim lcsu
       , Fractional n )
    => n -> unit -> Dim n dim lcsu
(%) = dimOf

defaultLCSU :: Dim n dim DefaultLCSU -> Dim n dim DefaultLCSU
defaultLCSU = id

-- | The number 1, expressed as a unitless dimensioned quantity.
unity :: Num n => Dim n '[] l
unity = Dim 1

-- | The number 0, polymorphic in its dimension. Use of this will
-- often require a type annotation.
zero :: Num n => Dim n dimspec l
zero = Dim 0

-- | Dimension-safe cast. See the README for more info.
dim :: (d @~ e) => Dim n d l -> Dim n e l
dim (Dim x) = Dim x

-------------------------------------------------------------
--- "Number" unit -------------------------------------------
-------------------------------------------------------------

-- | The dimension for the dimensionless quantities.
-- It is also called "quantities of dimension one", but
-- @One@ is confusing with the type-level integer One.
data Dimensionless = Dimensionless
instance Dimension Dimensionless where
  type DimSpecsOf Dimensionless = '[]

-- | The unit for unitless dimensioned quantities
data Number = Number -- the unit for unadorned numbers
instance Unit Number where
  type BaseUnit Number = Canonical
  type DimOfUnit Number = Dimensionless
  type UnitSpecsOf Number = '[]

-- | The type of unitless dimensioned quantities
-- This is an instance of @Num@, though Haddock doesn't show it.
type Scalar l = MkDim Number l

-- | Convert a raw number into a unitless dimensioned quantity
scalar :: n -> Dim n '[] l
scalar = Dim
