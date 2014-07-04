-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.NoVector.Poly
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This file exports the same definitions as Data.Metrology.Poly, but it
-- restricts arithmetic operators to just use the Prelude numerical hierarchy,
-- instead of vector-space. It uses orphan instances for vector-space classes
-- to do this, so this module is not generally suitable for use in a released
-- library. On the other hand, using these definitions may be convenient for
-- quick experimentation, because the vector-space classes interfere with
-- GHC's ambiguity-resolution for overloaded numbers.
-----------------------------------------------------------------------------

{-# LANGUAGE TypeOperators, ConstraintKinds, ScopedTypeVariables, DataKinds #-}

module Data.Metrology.NoVector.Poly (
  -- * Term-level combinators

  -- | The term-level arithmetic operators are defined by
  -- applying vertical bar(s) to the sides the dimensioned 
  -- quantities acts on.

  -- ** Additive operations
  zero, (|+|), (|-|), qSum, qNegate,

  -- ** Multiplicative operations between quantities
  (|*|), (|/|), (/|),

  -- ** Multiplicative operations between a quantity and a non-quantity
  (*|), (|*), (|/),

  -- ** Exponentiation
  (|^), (|^^), qNthRoot,
  qSq, qCube, qSqrt, qCubeRoot,

  -- ** Comparison
  qCompare, (|<|), (|>|), (|<=|), (|>=|), (|==|), (|/=|),
  qApprox, qNapprox,        

  -- * Conversion between quantities and numeric values
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

  -- * Internal definitions
  -- | The following module is re-exported solely to prevent noise in error messages;
  -- we do not recommend trying to use these definitions in user code.
  module Data.Metrology.Internal
  ) where

import Data.Metrology.Poly
  hiding ( zero, (|+|), (|-|), qSum, qNegate, (*|), (|*), (|/)
         , numIn, (#), quOf, (%), showIn, convert, constant )
import Data.Metrology.Qu  ( Qu(Qu) )
import Data.Metrology.Internal
import Data.Foldable as F
import Data.Proxy
import Data.Metrology.Units

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

