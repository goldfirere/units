{-# LANGUAGE TypeOperators, FlexibleContexts, DataKinds, TypeFamilies,
             ScopedTypeVariables, ConstraintKinds, GeneralizedNewtypeDeriving #-}

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
  -- * Multiplicative operations
  (|*^|), (|^*|), (|^/|), (|.|),

  -- * Other vector operations
  qMagnitudeSq, qMagnitude, qNormalized, qProject, qCross2, qCross3,

  -- * Affine operations
  Point(..), QPoint, (|.-.|), (|.+^|), (|.-^|), qDistanceSq, qDistance
  ) where

import Data.Metrology.Z as Z
import Data.Metrology.Quantity
import Data.Metrology.Factor

import Data.VectorSpace
import Data.Cross
import Data.AffineSpace

import Data.Coerce

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
