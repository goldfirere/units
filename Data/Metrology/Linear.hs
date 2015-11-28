{-# LANGUAGE TypeOperators, FlexibleContexts, DataKinds, TypeFamilies,
             ScopedTypeVariables, ConstraintKinds, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, FlexibleInstances, InstanceSigs #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Linear
-- Copyright   :  (C) 2014 Richard Eisenberg, (C) 2015 Tobias Markus
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports combinators for building quantities out of vectors, from the
-- @linear@ library.
------------------------------------------------------------------------------

module Data.Metrology.Linear (
  -- * Term-level combinators

  -- | The term-level arithmetic operators are defined by
  -- applying vertical bar(s) to the sides the dimensioned
  -- quantities acts on.

  -- ** Additive operations
  zeroV, (|^+^|), (|^-^|), qNegateV, qSumV,

  -- ** Multiplicative operations
  (|*^|), (|^*|), (|^/|), (*^|), (|^*), (|^/), (|.|),

  -- ** Vector-space operations
  qBasis, qBasisFor, qScaled, qOuter, qUnit,
  qQuadrance, qNorm, qSignorm, qProject, qCross,

  -- ** Affine operations
  (|.-.|), (|.+^|), (|.-^|), qQd, qDistance,

  -- * Nondimensional units, conversion between quantities and numeric values
  numInV, (^#), quOfV, (^%), showInV,
  convertV, constantV,

  ) where

import Data.Metrology.Qu
import Data.Metrology.LCSU
import Data.Metrology.Validity
import Data.Metrology.Factor
import Data.Metrology.Z as Z
import Data.Metrology.Units

import Linear
import Linear.Affine hiding (P)
import qualified Control.Lens as Lens

import Data.Proxy
import Data.Foldable    as F
import Data.Traversable as T

---------------------------------------
-- Additive operations
---------------------------------------

-- | The number 0, polymorphic in its dimension. Use of this will
-- often require a type annotation.
zeroV :: (Additive f, Num a) => Qu d l (f a)
zeroV = Qu Linear.zero

infixl 6 |^+^|
-- | Add two compatible vector quantities
(|^+^|) :: (d1 @~ d2, Additive f, Num a)
        => Qu d1 l (f a) -> Qu d2 l (f a) -> Qu d1 l (f a)
(Qu a) |^+^| (Qu b) = Qu (a ^+^ b)

-- | Negate a vector quantity
qNegateV :: (Additive f, Num a) => Qu d l (f a) -> Qu d l (f a)
qNegateV (Qu x) = Qu (negated x)

infixl 6 |^-^|
-- | Subtract two compatible quantities
(|^-^|) :: (d1 @~ d2, Additive f, Num a)
        => Qu d1 l (f a) -> Qu d2 l (f a) -> Qu d1 l (f a)
(Qu a) |^-^| (Qu b) = Qu (a ^-^ b)

-- | Take the sum of a list of quantities
qSumV :: (Foldable t, Additive f, Num a) => t (Qu d l (f a)) -> Qu d l (f a)
qSumV = F.foldr (|^+^|) zeroV

---------------------------------------
-- Multiplicative operations
---------------------------------------

infixl 7 |*^|, |^*|, |^/|
-- | Multiply a scalar quantity by a vector quantity
(|*^|) :: (Functor f, Num a)
       => Qu d1 l a -> Qu d2 l (f a) -> Qu (Normalize (d1 @+ d2)) l (f a)
(Qu a) |*^| (Qu b) = Qu (a *^ b)

-- | Multiply a vector quantity by a scalar quantity
(|^*|) :: (Functor f, Num a)
       => Qu d1 l (f a) -> Qu d2 l a -> Qu (Normalize (d1 @+ d2)) l (f a)
(Qu a) |^*| (Qu b) = Qu (a ^* b)

-- | Divide a vector quantity by a scalar quantity
(|^/|) :: (Functor f, Fractional a)
       => Qu d1 l (f a) -> Qu d2 l a -> Qu (Normalize (d1 @- d2)) l (f a)
(Qu a) |^/| (Qu b) = Qu (a ^/ b)

infixl 7 |^/
-- | Divide a quantity by a plain old number
(|^/) :: (Functor f, Fractional a) => Qu d l (f a) -> a -> Qu d l (f a)
(Qu a) |^/ b = Qu (a ^/ b)

infixl 7 *^| , |^*
-- | Multiply a quantity by a plain old number from the left
(*^|) :: (Functor f, Num a) => a -> Qu b l (f a) -> Qu b l (f a)
a *^| (Qu b) =  Qu (a *^ b)

-- | Multiply a quantity by a plain old number from the right
(|^*) :: (Functor f, Num a) => Qu b l (f a) -> a -> Qu b l (f a)
(Qu a) |^* b = Qu (a ^* b)

---------------------------------------
-- Vector-space operations
---------------------------------------

-- | Return a default basis, where each basis element measures 1 of the
-- unit provided.
qBasis :: ( ValidDLU dim lcsu unit
          , Additive f
          , Traversable f
          , Fractional a )
       => unit -> [Qu dim lcsu (f a)]
qBasis u = map (^% u) basis

-- | Return a default basis for the vector space provided. Each basis
-- element measures 1 of the unit provided.
qBasisFor :: ( ValidDLU dim lcsu unit
             , Additive f
             , Traversable f
             , Fractional a )
          => unit -> Qu dim lcsu (f b) -> [Qu dim lcsu (f a)]
qBasisFor u (Qu vec) = map (^% u) (basisFor vec)

-- | Produce a diagonal (scale) matrix from a vector
qScaled :: (Traversable f, Num a)
        => Qu dim lcsu (f a) -> Qu dim lcsu (f (f a))
qScaled (Qu vec) = Qu (scaled vec)

-- | Outer (tensor) product of two quantity vectors
qOuter :: (Functor f, Functor g, Num a)
       => Qu d1 l (f a) -> Qu d2 l (g a) -> Qu (Normalize (d1 @+ d2)) l (f (g a))
qOuter (Qu a) (Qu b) = Qu (a `outer` b)

-- | Create a unit vector from a setter and a choice of unit.
qUnit :: (ValidDLU dim lcsu unit, Additive t, Fractional a)
      => Lens.ASetter' (t a) a -> unit -> Qu dim lcsu (t a)
qUnit setter u = unit setter ^% u

infixl 7 |.|
-- | Take a inner (dot) product between two quantities.
(|.|) :: (Metric f, Num a) => Qu d1 l (f a) -> Qu d2 l (f a) -> Qu (Normalize (d1 @+ d2)) l a
(Qu a) |.| (Qu b) = Qu (a `dot` b)

-- | Square the length of a vector.
qQuadrance :: (Metric f, Num a) => Qu d l (f a) -> Qu (d @* Z.Two) l a
qQuadrance (Qu x) = Qu (quadrance x)

-- | Length of a vector.
qNorm :: (Metric f, Floating a) => Qu d l (f a) -> Qu d l a
qNorm (Qu x) = Qu (norm x)

-- | Vector in same direction as given one but with length of one. If given the zero
-- vector, then return it. The returned vector is dimensionless.
qSignorm :: (Metric f, Floating a)
         => Qu d l (f a) -> Qu '[] l (f a)
qSignorm (Qu x) = Qu (signorm x)

-- | @qProject u v@ computes the projection of @v@ onto @u@.
qProject :: (Metric f, Fractional a)
         => Qu d2 l (f a) -> Qu d1 l (f a) -> Qu d1 l (f a)
qProject (Qu u) (Qu v) = Qu (u `project` v)

-- | Cross product of 3D vectors.
qCross :: Num a
       => Qu d1 l (V3 a) -> Qu d2 l (V3 a) -> Qu (Normalize (d1 @+ d2)) l (V3 a)
qCross (Qu x) (Qu y) = Qu (x `cross` y)

-- | Square of the distance between two vectors.
qQd :: (d1 @~ d2, Metric f, Metric (Diff f), Num a)
            => Qu d1 l (f a) -> Qu d2 l (f a) -> Qu (d1 @* Z.Two) l a
qQd (Qu a) (Qu b) = Qu (a `qd` b)

-- | Distance between two vectors.
qDistance :: (d1 @~ d2, Metric f, Metric (Diff f), Floating a)
          => Qu d1 l (f a) -> Qu d2 l (f a) -> Qu d1 l a
qDistance (Qu a) (Qu b) = Qu (a `distance` b)

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
qQdA :: (d1 @~ d2, Affine f, Foldable (Diff f), Num a)
            => Qu d1 l (f a) -> Qu d2 l (f a) -> Qu (d1 @* Z.Two) l a
qQdA (Qu a) (Qu b) = Qu (a `qdA` b)

-- | Distance between two points.
qDistanceA :: (d1 @~ d2, Affine f, Foldable (Diff f), Floating a)
          => Qu d1 l (f a) -> Qu d2 l (f a) -> Qu d1 l a
qDistanceA (Qu a) (Qu b) = Qu (a `distanceA` b)

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
numInV :: forall unit dim lcsu f a.
         ( ValidDLU dim lcsu unit
         , Functor f
         , Fractional a )
      => Qu dim lcsu (f a) -> unit -> (f a)
numInV (Qu val) u
  = val ^* fromRational
             (canonicalConvRatioSpec (Proxy :: Proxy (LookupList dim lcsu))
              / canonicalConvRatio u)

infix 5 ^#
-- | Infix synonym for 'numIn'
(^#) :: ( ValidDLU dim lcsu unit
         , Functor f
         , Fractional a )
    => Qu dim lcsu (f a) -> unit -> (f a)
(^#) = numInV

-- | Creates a dimensioned quantity in the given unit. For example:
--
--   > height :: Length
--   > height = quOf 2.0 Meter
--
--   or
--
--   > height = 2.0 % Meter
quOfV :: forall unit dim lcsu f a.
         ( ValidDLU dim lcsu unit
         , Functor f
         , Fractional a )
      => (f a) -> unit -> Qu dim lcsu (f a)
quOfV d u
  = Qu (d ^* fromRational
               (canonicalConvRatio u
                / canonicalConvRatioSpec (Proxy :: Proxy (LookupList dim lcsu))))

infixr 9 ^%
-- | Infix synonym for 'quOf'
(^%) :: ( ValidDLU dim lcsu unit
         , Functor f
         , Fractional a )
    => (f a) -> unit -> Qu dim lcsu (f a)
(^%) = quOfV

-- | Dimension-keeping cast between different CSUs.
convertV :: forall d l1 l2 f a.
  ( ConvertibleLCSUs d l1 l2
  , Functor f
  , Fractional a )
  => Qu d l1 (f a) -> Qu d l2 (f a)
convertV (Qu x) = Qu $ x ^* fromRational (
  canonicalConvRatioSpec (Proxy :: Proxy (LookupList d l1))
  / canonicalConvRatioSpec (Proxy :: Proxy (LookupList d l2)))


-- | Compute the argument in the @DefaultLCSU@, and present the result as
-- lcsu-polymorphic dimension-polymorphic value. Named 'constant' because one
-- of its dominant usecase is to inject constant quantities into
-- dimension-polymorphic expressions.
constantV :: ( d @~ e
            , ConvertibleLCSUs e DefaultLCSU l
            , Functor f
            , Fractional a )
         => Qu d DefaultLCSU (f a) -> Qu e l (f a)
constantV = convertV . redim

infix 1 `showInV`
-- | Show a dimensioned quantity in a given unit. (The default @Show@
-- instance always uses units as specified in the LCSU.)
showInV :: ( ValidDLU dim lcsu unit
          , Functor f
          , Fractional a
          , Show unit
          , Show a
          , Show (f a) )
       => Qu dim lcsu (f a) -> unit -> String
showInV x u = show (x ^# u) ++ " " ++ show u
