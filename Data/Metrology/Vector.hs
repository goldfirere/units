{-# LANGUAGE TypeOperators, FlexibleContexts, DataKinds, TypeFamilies, CPP,
             ScopedTypeVariables, ConstraintKinds, GeneralizedNewtypeDeriving #-}

#if __GLASGOW_HASKELL__ >= 711
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Vector
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports combinators for building quantities out of vectors, from the
-- vector-space library.
------------------------------------------------------------------------------

module Data.Metrology.Vector (
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

  -- ** Multiplicative operations on vectors
  (|*^|), (|^*|), (|^/|), (|.|),

  -- ** Exponentiation
  (|^), (|^^), qNthRoot,
  qSq, qCube, qSqrt, qCubeRoot,

  -- ** Other vector operations
  qMagnitudeSq, qMagnitude, qNormalized, qProject, qCross2, qCross3,

  -- ** Affine operations
  Point(..), QPoint, (|.-.|), (|.+^|), (|.-^|), qDistanceSq, qDistance,
  pointNumIn, (.#), quOfPoint, (%.),

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

import Data.AffineSpace
import Data.VectorSpace
import Data.Cross hiding ( One, Two, Three )

import Data.Proxy
import Data.Coerce
import Data.Foldable as F

---------------------------------------
-- Additive operations
---------------------------------------

-- | The number 0, polymorphic in its dimension. Use of this will
-- often require a type annotation.
zero :: AdditiveGroup n => Qu dimspec l n
zero = Qu zeroV

infixl 6 |+|
-- | Add two compatible quantities
(|+|) :: (d1 @~ d2, AdditiveGroup n) => Qu d1 l n -> Qu d2 l n -> Qu d1 l n
(Qu a) |+| (Qu b) = Qu (a ^+^ b)

-- | Negate a quantity
qNegate :: AdditiveGroup n => Qu d l n -> Qu d l n
qNegate (Qu x) = Qu (negateV x)

infixl 6 |-|
-- | Subtract two compatible quantities
(|-|) :: (d1 @~ d2, AdditiveGroup n) => Qu d1 l n -> Qu d2 l n -> Qu d1 l n
a |-| b = a |+| qNegate b

-- | Take the sum of a list of quantities
qSum :: (Foldable f, AdditiveGroup n) => f (Qu d l n) -> Qu d l n
qSum = F.foldr (|+|) zero

---------------------------------------
-- Vector multiplicative operations
---------------------------------------

infixl 7 |*^|, |^*|, |^/|
-- | Multiply a scalar quantity by a vector quantity
(|*^|) :: VectorSpace n => Qu d1 l (Scalar n) -> Qu d2 l n -> Qu (Normalize (d1 @+ d2)) l n
(Qu a) |*^| (Qu b) = Qu (a *^ b)

-- | Multiply a vector quantity by a scalar quantity
(|^*|) :: VectorSpace n => Qu d1 l n -> Qu d2 l (Scalar n) -> Qu (Normalize (d1 @+ d2)) l n
(Qu a) |^*| (Qu b) = Qu (a ^* b)

-- | Divide a vector quantity by a scalar quantity
(|^/|) :: (VectorSpace n, Fractional (Scalar n))
       => Qu d1 l n -> Qu d2 l (Scalar n) -> Qu (Normalize (d1 @- d2)) l n
(Qu a) |^/| (Qu b) = Qu (a ^/ b)

infixl 7 |/
-- | Divide a quantity by a scalar
(|/) :: (VectorSpace n, Fractional (Scalar n)) => Qu a l n -> Scalar n -> Qu a l n
(Qu a) |/ b = Qu (a ^/ b)
-- The above function should *not* need to be privileged. But, GHC can't figure
-- out that a @@- '[] ~ a. Urgh.

infixl 7 *| , |*
-- | Multiply a quantity by a scalar from the left
(*|) :: VectorSpace n => Scalar n -> Qu b l n -> Qu b l n
a *| (Qu b) =  Qu (a *^ b)

-- | Multiply a quantity by a scalar from the right
(|*) :: VectorSpace n => Qu a l n -> Scalar n -> Qu a l n
(Qu a) |* b = Qu (a ^* b)

---------------------------------------
-- Multiplicative operations
---------------------------------------

infixl 7 |.|
-- | Take a inner (dot) product between two quantities.
(|.|) :: InnerSpace n => Qu d1 l n -> Qu d2 l n -> Qu (Normalize (d1 @+ d2)) l (Scalar n)
(Qu a) |.| (Qu b) = Qu (a <.> b)

-- | Square the length of a vector.
qMagnitudeSq :: InnerSpace n => Qu d l n -> Qu (d @* Z.Two) l (Scalar n)
qMagnitudeSq (Qu x) = Qu (magnitudeSq x)

-- | Length of a vector.
qMagnitude :: (InnerSpace n, Floating (Scalar n)) => Qu d l n -> Qu d l (Scalar n)
qMagnitude (Qu x) = Qu (magnitude x)

-- | Vector in same direction as given one but with length of one. If given the zero
-- vector, then return it. The returned vector is dimensionless.
qNormalized :: (InnerSpace n, Floating (Scalar n)) => Qu d l n -> Qu '[] l n
qNormalized (Qu x) = Qu (normalized x)

-- | @qProject u v@ computes the projection of @v@ onto @u@.
qProject :: (InnerSpace n, Floating (Scalar n)) => Qu d2 l n -> Qu d1 l n -> Qu d1 l n
qProject (Qu u) (Qu v) = Qu (u `project` v)

-- | Cross product of 2D vectors.
qCross2 :: HasCross2 n => Qu d l n -> Qu d l n
qCross2 (Qu x) = Qu (cross2 x)

-- | Cross product of 3D vectors.
qCross3 :: HasCross3 n => Qu d1 l n -> Qu d2 l n -> Qu (Normalize (d1 @+ d2)) l n
qCross3 (Qu x) (Qu y) = Qu (x `cross3` y)

---------------------------------------
-- Affine space operations
---------------------------------------

-- | A @Point n@ is an affine space built over @n@. Two @Point@s cannot be added,
-- but they can be subtracted to yield a difference of type @n@.
newtype Point n = Point n
  deriving (Show, Eq, Enum, Bounded)

-- | Make a point quantity from a non-point quantity.
type family QPoint n where
  QPoint (Qu d l n) = Qu d l (Point n)

instance AdditiveGroup n => AffineSpace (Point n) where
  type Diff (Point n) = n
  (.-.) = coerce ((^-^) :: n -> n -> n)
  (.+^) = coerce ((^+^) :: n -> n -> n)

-- | Make a point quantity at the given unit. Like 'quOf'.
quOfPoint :: forall dim lcsu unit n.
             ( ValidDLU dim lcsu unit
             , VectorSpace n
             , Fractional (Scalar n) )
          => n -> unit -> Qu dim lcsu (Point n)
quOfPoint n unit = Qu (Point x)
  where Qu x = quOf n unit :: Qu dim lcsu n

infix 5 %.
-- | Infix synonym of 'quOfPoint'
(%.) :: ( ValidDLU dim lcsu unit
        , VectorSpace n
        , Fractional (Scalar n) )
     => n -> unit -> Qu dim lcsu (Point n)
(%.) = quOfPoint

-- | Extract the numerical value from a point quantity. Like 'numIn'.
pointNumIn :: forall unit dim lcsu n.
              ( ValidDLU dim lcsu unit
              , VectorSpace n
              , Fractional (Scalar n) )
           => Qu dim lcsu (Point n) -> unit -> n
pointNumIn (Qu (Point n)) unit = numIn (Qu n :: Qu dim lcsu n) unit

infix 5 .#
-- | Infix synonym for 'pointNumIn'.
(.#) :: (ValidDLU dim lcsu unit, VectorSpace n, Fractional (Scalar n))
     => Qu dim lcsu (Point n) -> unit -> n
(.#) = pointNumIn

infixl 6 |.-.|, |.+^|, |.-^|

-- | Subtract point quantities.
(|.-.|) :: (d1 @~ d2, AffineSpace n) => Qu d1 l n -> Qu d2 l n -> Qu d1 l (Diff n)
(Qu a) |.-.| (Qu b) = Qu (a .-. b)

-- | Add a point to a vector.
(|.+^|) :: (d1 @~ d2, AffineSpace n) => Qu d1 l n -> Qu d2 l (Diff n) -> Qu d1 l n
(Qu a) |.+^| (Qu b) = Qu (a .+^ b)

-- | Subract a vector from a point.
(|.-^|) :: (d1 @~ d2, AffineSpace n) => Qu d1 l n -> Qu d2 l (Diff n) -> Qu d1 l n
(Qu a) |.-^| (Qu b) = Qu (a .-^ b)

-- | Square of the distance between two points.
qDistanceSq :: (d1 @~ d2, AffineSpace n, InnerSpace (Diff n))
            => Qu d1 l n -> Qu d2 l n -> Qu (d1 @* Z.Two) l (Scalar (Diff n))
qDistanceSq (Qu a) (Qu b) = Qu (a `distanceSq` b)

-- | Distance between two points.
qDistance :: (d1 @~ d2, AffineSpace n, InnerSpace (Diff n), Floating (Scalar (Diff n)))
          => Qu d1 l n -> Qu d2 l n -> Qu d1 l (Scalar (Diff n))
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

infix 5 %
-- | Infix synonym for 'quOf'
(%) :: ( ValidDLU dim lcsu unit
       , VectorSpace n
       , Fractional (Scalar n) )
    => n -> unit -> Qu dim lcsu n
(%) = quOf

-- | Dimension-keeping cast between different CSUs.
convert :: forall d l1 l2 n.
  ( ConvertibleLCSUs d l1 l2
  , VectorSpace n
  , Fractional (Scalar n) )
  => Qu d l1 n -> Qu d l2 n
convert (Qu x) = Qu $ x ^* fromRational (
  canonicalConvRatioSpec (Proxy :: Proxy (LookupList d l1))
  / canonicalConvRatioSpec (Proxy :: Proxy (LookupList d l2)))


-- | Compute the argument in the @DefaultLCSU@, and present the result as
-- lcsu-polymorphic dimension-polymorphic value. Named 'constant' because one
-- of its dominant usecase is to inject constant quantities into
-- dimension-polymorphic expressions.
constant :: ( d @~ e
            , ConvertibleLCSUs e DefaultLCSU l
            , VectorSpace n
            , Fractional (Scalar n) )
         => Qu d DefaultLCSU n -> Qu e l n
constant = convert . redim

infix 1 `showIn`
-- | Show a dimensioned quantity in a given unit. (The default @Show@
-- instance always uses units as specified in the LCSU.)
showIn :: ( ValidDLU dim lcsu unit
          , VectorSpace n
          , Fractional (Scalar n)
          , Show unit
          , Show n )
       => Qu dim lcsu n -> unit -> String
showIn x u = show (x # u) ++ " " ++ show u
