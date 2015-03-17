{-# LANGUAGE TypeOperators, FlexibleContexts, DataKinds, TypeFamilies,
             ScopedTypeVariables, ConstraintKinds, GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Vector
-- Copyright   :  (C) 2014 Richard Eisenberg, (C) 2015 Tobias Markus
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports combinators for building quantities out of vectors, from the
-- vector-space library.
------------------------------------------------------------------------------

module Data.Metrology.Linear (
  -- * Term-level combinators

  -- | The term-level arithmetic operators are defined by
  -- applying vertical bar(s) to the sides the dimensioned
  -- quantities acts on.

  -- ** Additive operations
  Data.Metrology.Linear.zero, (|+|), (|-|), qNegate, qSum,

  -- ** Multiplicative operations between non-vector quantities
  (|*|), (|/|), (/|),

  -- ** Multiplicative operations between a vector and a scalar
  (*|), (|*), (|/),

  -- ** Multiplicative operations on vectors
  (|*^|), (|^*|), (|^/|), (|.|),

  -- ** Exponentiation
  (|^), (|^^), qNthRoot,
  qSq, qCube, qSqrt, qCubeRoot,

  -- ** Other vector operations
  qMagnitudeSq, qMagnitude, qNormalized, qProject, qCross3,

  -- ** Affine operations
  (|.-.|), (|.+^|), (|.-^|), qDistanceSq, qDistance,

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

  -- * Internal definitions
  -- | The following module is re-exported solely to prevent noise in error messages;
  -- we do not recommend trying to use these definitions in user code.
  module Data.Metrology.Internal

  ) where

import Data.Metrology.Qu
import Data.Metrology.LCSU
import Data.Metrology.Validity
import Data.Metrology.Factor
import Data.Metrology.Z as Z
import Data.Metrology.Units
import Data.Metrology.Combinators
import Data.Metrology.Dimensions
import Data.Metrology.Internal

import Linear
import Linear.Affine hiding (P)

import Data.Proxy
import Data.Foldable as F

---------------------------------------
-- Additive operations
---------------------------------------

-- | The number 0, polymorphic in its dimension. Use of this will
-- often require a type annotation.
zero :: (Additive f, Num a) => Qu d l (f a)
zero = Qu Linear.zero

infixl 6 |+|
-- | Add two compatible quantities
(|+|) :: (d1 @~ d2, Additive f, Num a) => Qu d1 l (f a) -> Qu d2 l (f a) -> Qu d1 l (f a)
(Qu a) |+| (Qu b) = Qu (a ^+^ b)

-- | Negate a quantity
qNegate :: (Additive f, Num a) => Qu d l (f a) -> Qu d l (f a)
qNegate (Qu x) = Qu (negated x)

infixl 6 |-|
-- | Subtract two compatible quantities
(|-|) :: (d1 @~ d2, Additive f, Num a) => Qu d1 l (f a) -> Qu d2 l (f a) -> Qu d1 l (f a)
a |-| b = a |+| qNegate b

-- | Take the sum of a list of quantities
qSum :: (Foldable t, Additive f, Num a) => t (Qu d l (f a)) -> Qu d l (f a)
qSum = F.foldr (|+|) Data.Metrology.Linear.zero

---------------------------------------
-- Vector multiplicative operations
---------------------------------------

infixl 7 |*^|, |^*|, |^/|
-- | Multiply a scalar quantity by a vector quantity
(|*^|) :: (Functor f, Num a) => Qu d1 l a -> Qu d2 l (f a) -> Qu (Normalize (d1 @+ d2)) l (f a)
(Qu a) |*^| (Qu b) = Qu (a *^ b)

-- | Multiply a vector quantity by a scalar quantity
(|^*|) :: (Functor f, Num a) => Qu d1 l (f a) -> Qu d2 l a -> Qu (Normalize (d1 @+ d2)) l (f a)
(Qu a) |^*| (Qu b) = Qu (a ^* b)

-- | Divide a vector quantity by a scalar quantity
(|^/|) :: (Functor f, Fractional a) => Qu d1 l (f a) -> Qu d2 l a -> Qu (Normalize (d1 @- d2)) l (f a)
(Qu a) |^/| (Qu b) = Qu (a ^/ b)

infixl 7 |/
-- | Divide a quantity by a scalar
(|/) :: (Functor f, Fractional a) => Qu d l (f a) -> a -> Qu d l (f a)
(Qu a) |/ b = Qu (a ^/ b)
-- The above function should *not* need to be privileged. But, GHC can't figure
-- out that a @@- '[] ~ a. Urgh.

infixl 7 *| , |*
-- | Multiply a quantity by a scalar from the left
(*|) :: (Functor f, Num a) => a -> Qu b l (f a) -> Qu b l (f a)
a *| (Qu b) =  Qu (a *^ b)

-- | Multiply a quantity by a scalar from the right
(|*) :: (Functor f, Num a) => Qu b l (f a) -> a -> Qu b l (f a)
(Qu a) |* b = Qu (a ^* b)

---------------------------------------
-- Multiplicative operations
---------------------------------------

infixl 7 |.|
-- | Take a inner (dot) product between two quantities.
(|.|) :: (Metric f, Num a) => Qu d1 l (f a) -> Qu d2 l (f a) -> Qu (Normalize (d1 @+ d2)) l a
(Qu a) |.| (Qu b) = Qu (a `dot` b)

-- | Square the length of a vector.
qMagnitudeSq :: (Metric f, Num a) => Qu d l (f a) -> Qu (d @* Z.Two) l a
qMagnitudeSq (Qu x) = Qu (quadrance x)

-- | Length of a vector.
qMagnitude :: (Metric f, Floating a) => Qu d l (f a) -> Qu d l a
qMagnitude (Qu x) = Qu (norm x)

-- | Vector in same direction as given one but with length of one. If given the zero
-- vector, then return it. The returned vector is dimensionless.
qNormalized :: (Metric f, Floating a) => Qu d l (f a) -> Qu '[] l (f a)
qNormalized (Qu x) = Qu (signorm x)

-- | @qProject u v@ computes the projection of @v@ onto @u@.
qProject :: (Metric f, Fractional a) => Qu d2 l (f a) -> Qu d1 l (f a) -> Qu d1 l (f a)
qProject (Qu u) (Qu v) = Qu (u `project` v)

-- | Cross product of 3D vectors.
qCross3 :: Num a => Qu d1 l (V3 a) -> Qu d2 l (V3 a) -> Qu (Normalize (d1 @+ d2)) l (V3 a)
qCross3 (Qu x) (Qu y) = Qu (x `cross` y)

---------------------------------------
-- Affine space operations
---------------------------------------

-- | Subtract point quantities.
(|.-.|) :: (d1 @~ d2, Affine f, Num a) => Qu d1 l (f a) -> Qu d2 l (f a) -> Qu d1 l (Diff f a)
(Qu a) |.-.| (Qu b) = Qu (a .-. b)

-- | Add a point to a vector.
(|.+^|) :: (d1 @~ d2, Affine f, Num a) => Qu d1 l (f a) -> Qu d2 l (Diff f a) -> Qu d1 l (f a)
(Qu a) |.+^| (Qu b) = Qu (a .+^ b)

-- | Subract a vector from a point.
(|.-^|) :: (d1 @~ d2, Affine f, Num a) => Qu d1 l (f a) -> Qu d2 l (Diff f a) -> Qu d1 l (f a)
(Qu a) |.-^| (Qu b) = Qu (a .-^ b)

-- | Square of the distance between two points.
qDistanceSq :: (d1 @~ d2, Metric f, Metric (Diff f), Num a)
            => Qu d1 l (f a) -> Qu d2 l (f a) -> Qu (d1 @* Z.Two) l a
qDistanceSq (Qu a) (Qu b) = Qu (a `qd` b)

-- | Distance between two points.
qDistance :: (d1 @~ d2, Metric f, Metric (Diff f), Floating a)
          => Qu d1 l (f a) -> Qu d2 l (f a) -> Qu d1 l a
qDistance (Qu a) (Qu b) = Qu (a `distance` b)

---------------------------------------
-- Top-level operations
---------------------------------------

-- | Extracts a numerical value from a dimensioned quantity, expressed in
--   the given unit. For example:
--
--   > inMeters :: Length -> Double
--   > inMeters x = numIn x Meter
--
--   or
--
--   > inMeters x = x # Meter
numIn :: forall unit dim lcsu f a.
         ( ValidDLU dim lcsu unit
         , Functor f
         , Fractional a )
      => Qu dim lcsu (f a) -> unit -> (f a)
numIn (Qu val) u
  = val ^* fromRational
             (canonicalConvRatioSpec (Proxy :: Proxy (LookupList dim lcsu))
              / canonicalConvRatio u)

infix 5 #
-- | Infix synonym for 'numIn'
(#) :: ( ValidDLU dim lcsu unit
         , Functor f
         , Fractional a )
    => Qu dim lcsu (f a) -> unit -> (f a)
(#) = numIn

-- | Creates a dimensioned quantity in the given unit. For example:
--
--   > height :: Length
--   > height = quOf 2.0 Meter
--
--   or
--
--   > height = 2.0 % Meter
quOf :: forall unit dim lcsu f a.
         ( ValidDLU dim lcsu unit
         , Functor f
         , Fractional a )
      => (f a) -> unit -> Qu dim lcsu (f a)
quOf d u
  = Qu (d ^* fromRational
               (canonicalConvRatio u
                / canonicalConvRatioSpec (Proxy :: Proxy (LookupList dim lcsu))))

infixr 9 %
-- | Infix synonym for 'quOf'
(%) :: ( ValidDLU dim lcsu unit
         , Functor f
         , Fractional a )
    => (f a) -> unit -> Qu dim lcsu (f a)
(%) = quOf

-- | Dimension-keeping cast between different CSUs.
convert :: forall d l1 l2 f a.
  ( ConvertibleLCSUs d l1 l2
  , Functor f
  , Fractional a )
  => Qu d l1 (f a) -> Qu d l2 (f a)
convert (Qu x) = Qu $ x ^* fromRational (
  canonicalConvRatioSpec (Proxy :: Proxy (LookupList d l1))
  / canonicalConvRatioSpec (Proxy :: Proxy (LookupList d l2)))


-- | Compute the argument in the @DefaultLCSU@, and present the result as
-- lcsu-polymorphic dimension-polymorphic value. Named 'constant' because one
-- of its dominant usecase is to inject constant quantities into
-- dimension-polymorphic expressions.
constant :: ( d @~ e
            , ConvertibleLCSUs e DefaultLCSU l
            , Functor f
            , Fractional a )
         => Qu d DefaultLCSU (f a) -> Qu e l (f a)
constant = convert . redim

infix 1 `showIn`
-- | Show a dimensioned quantity in a given unit. (The default @Show@
-- instance always uses units as specified in the LCSU.)
showIn :: ( ValidDLU dim lcsu unit
          , Functor f
          , Fractional a
          , Show unit
          , Show a
          , Show (f a) )
       => Qu dim lcsu (f a) -> unit -> String
showIn x u = show (x # u) ++ " " ++ show u
