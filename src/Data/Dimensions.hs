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
             TypeOperators, ConstraintKinds #-}

module Data.Dimensions (
  Z(..), Succ, Pred, type (#+), type (#-), type (#*), type (#/), NegZ,
  One, Two, Three, Four, Five, MOne, MTwo, MThree, MFour, MFive,
  pZero, pOne, pTwo, pThree, pFour, pFive,
  pMOne, pMTwo, pMThree, pMFour, pMFive,
  pSucc, pPred,

  (.+), (.-), (.*), (./), (.^), (*.),
  (.<), (.>), (.<=), (.>=), dimEq, dimNeq,
  nthRoot, dimSqrt, dimCubeRoot,
  unity, zero, dim,

  Canonical, Unit(type BaseUnit, conversionRatio),

  MkDim, dimIn, (#), dimOf, (%),

  (:*)(..), (:/)(..), (:^)(..), (:@)(..),
  type (%*), type (%/), type (%^),
  UnitPrefix(..),

  Number(..), Scalar

  ) where

import Data.Dimensions.Z
import Data.Dimensions.Dim
import Data.Dimensions.DimSpec
import Data.Dimensions.Units
import Data.Dimensions.UnitCombinators
import Data.Dimensions.Show ()

dimIn :: Unit unit => MkDim (CanonicalUnit unit) -> unit -> Double
dimIn (Dim val) u = val / canonicalConvRatio u

infix 5 #
(#) :: Unit unit => MkDim (CanonicalUnit unit) -> unit -> Double
(#) = dimIn

dimOf :: Unit unit => Double -> unit -> MkDim (CanonicalUnit unit)
dimOf d u = Dim (d * canonicalConvRatio u)

infix 9 %
(%) :: Unit unit => Double -> unit -> MkDim (CanonicalUnit unit)
(%) = dimOf

unity :: Dim '[]
unity = Dim 1

zero :: Dim '[DAny]
zero = Dim 0

-- Dimension cast, with a short, convenient name.
dim :: (d @~ e) => Dim d -> Dim e
dim (Dim x) = Dim x

-------------------------------------------------------------
--- "Number" unit -------------------------------------------
-------------------------------------------------------------

data Number = Number -- the unit for unadorned numbers
instance Unit Number where
  type BaseUnit Number = Canonical
  type DimSpecsOf Number = '[]

type Scalar = MkDim Number