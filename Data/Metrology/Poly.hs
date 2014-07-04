{- Data/Metrology/Poly.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file gathers and exports all user-visible pieces of the units package.
   It also defines the main creators and consumers of dimensioned objects.
-}

{-# LANGUAGE ExplicitNamespaces, DataKinds, FlexibleInstances, TypeFamilies,
             TypeOperators, ConstraintKinds, ScopedTypeVariables,
             FlexibleContexts, UndecidableInstances #-}

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
-- applications. See also 'Data.Metrology.Vector' for polymorphic
-- functions suitable for use with the numerical classes from the
-- @vector-space@ package.
-----------------------------------------------------------------------------

module Data.Metrology.Poly (
  -- * Term-level combinators

  -- | The term-level arithmetic operators are defined by
  -- applying vertical bar(s) to the sides the dimensioned 
  -- quantities acts on.

  -- ** Additive operations
  zero, (|+|), (|-|), qSum, qNegate,

  -- ** Multiplicative operations between quantities
  (|*|), (|/|),

  -- ** Multiplicative operations between a quantity and a non-quantity
  (*|), (|*), (/|), (|/),

  -- ** Exponentiation
  (|^), (|^^), qNthRoot,
  qSq, qCube, qSqrt, qCubeRoot,

  -- ** Comparison
  qCompare, (|<|), (|>|), (|<=|), (|>=|), (|==|), (|/=|),
  qApprox, qNapprox,        

  -- * Nondimensional units, conversion between quantities and numeric values
  numIn, (#), quOf, (%), showIn,
  unity, redim, convert,
  defaultLCSU, constant, 

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
  MultDimFactors, MultUnitFactors, UnitOfDimFactors,

  -- * Type-level integers
  Z(..), Succ, Pred, type (#+), type (#-), type (#*), type (#/), Negate,

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
import Data.Metrology.Qu
import Data.Metrology.Dimensions
import Data.Metrology.Factor
import Data.Metrology.Units
import Data.Metrology.Combinators
import Data.Metrology.LCSU
import Data.Metrology.Validity
import Data.Metrology.Internal

import Data.Foldable as F
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

infix 1 `showIn`
-- | Show a dimensioned quantity in a given unit. (The default @Show@
-- instance always uses units as specified in the LCSU.)
showIn :: ( ValidDLU dim lcsu unit
          , Fractional n
          , Show unit
          , Show n )
       => Qu dim lcsu n -> unit -> String
showIn x u = show (x # u) ++ " " ++ show u

-- | Dimension-keeping cast between different CSUs.
convert :: forall d l1 l2 n. 
  ( ConvertibleLCSUs d l1 l2
  , Fractional n )
  => Qu d l1 n -> Qu d l2 n
convert (Qu x) = Qu $ x * fromRational (
  canonicalConvRatioSpec (Proxy :: Proxy (LookupList d l1))
  / canonicalConvRatioSpec (Proxy :: Proxy (LookupList d l2)))

-- | Compute the argument in the @DefaultLCSU@, and present the result as
-- lcsu-polymorphic dimension-polymorphic value. Named 'constant' because one
-- of its dominant usecase is to inject constant quantities into
-- dimension-polymorphic expressions.
constant :: ( d @~ e
            , ConvertibleLCSUs e DefaultLCSU l
            , Fractional n )
         => Qu d DefaultLCSU n -> Qu e l n
constant = convert . redim

----------------------------------------------------
-- Qu operations
----------------------------------------------------

-- | The number 0, polymorphic in its dimension. Use of this will
-- often require a type annotation.
zero :: Num n => Qu dimspec l n
zero = Qu 0

infixl 6 |+|
-- | Add two compatible quantities
(|+|) :: (d1 @~ d2, Num n) => Qu d1 l n -> Qu d2 l n -> Qu d1 l n
(Qu a) |+| (Qu b) = Qu (a + b)

infixl 6 |-|
-- | Subtract two compatible quantities
(|-|) :: (d1 @~ d2, Num n) => Qu d1 l n -> Qu d2 l n -> Qu d1 l n
a |-| b = a |+| qNegate b

-- | Take the sum of a list of quantities
qSum :: (Foldable f, Num n) => f (Qu d l n) -> Qu d l n
qSum = F.foldr (|+|) zero

-- | Negate a quantity
qNegate :: Num n => Qu d l n -> Qu d l n
qNegate (Qu x) = Qu (negate x)

infixl 7 *| , |* , |/
-- | Multiply a quantity by a scalar from the left
(*|) :: Num n => n -> Qu b l n -> Qu (Normalize b) l n
a *| (Qu b) = Qu (a * b)

-- | Multiply a quantity by a scalar from the right
(|*) :: Num n => Qu a l n -> n -> Qu (Normalize a) l n
(Qu a) |* b = Qu (a * b)

-- | Divide a quantity by a scalar
(|/) :: Fractional n => Qu a l n -> n -> Qu (Normalize a) l n
(Qu a) |/ b = Qu (a / b)

