{- Data/Dimensions.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file defines the Dim type and operations on that type.
-}

{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds, UndecidableInstances,
             ConstraintKinds, StandaloneDeriving, GeneralizedNewtypeDeriving,
             FlexibleInstances #-}

module Data.Dimensions.Dim where

import GHC.TypeLits ( Sing )

import Data.Dimensions.DimSpec
import Data.Dimensions.Z

-------------------------------------------------------------
--- Internal ------------------------------------------------
-------------------------------------------------------------

-- Dim adds a dimensional annotation to a Double
newtype Dim (a :: [DimSpec *]) = Dim Double

-------------------------------------------------------------
--- User-facing ---------------------------------------------
-------------------------------------------------------------

infixl 6 .+
(.+) :: (d1 @~ d2) => Dim d1 -> Dim d2 -> Dim (ChooseFrom d1 d2)
(Dim a) .+ (Dim b) = Dim (a + b)

infixl 6 .-
(.-) :: (d1 @~ d2) => Dim d1 -> Dim d2 -> Dim (ChooseFrom d1 d2)
(Dim a) .- (Dim b) = Dim (a - b)

infixl 7 .*
(.*) :: Dim a -> Dim b -> Dim (Normalize (a @+ b))
(Dim a) .* (Dim b) = Dim (a * b)

infixl 7 ./
(./) :: Dim a -> Dim b -> Dim (Normalize (a @- b))
(Dim a) ./ (Dim b) = Dim (a / b)

infixr 8 .^
(.^) :: Dim a -> Sing z -> Dim (a @* z)
(Dim a) .^ sz = Dim (a ^^ szToInt sz)

nthRoot :: (Zero < z) ~ True => Sing z -> Dim a -> Dim (a @/ z)
nthRoot sz (Dim a) = Dim (a ** (1.0 / (fromIntegral $ szToInt sz)))

infix 4 .<
(.<) :: (d1 @~ d2) => Dim d1 -> Dim d2 -> Bool
(Dim a) .< (Dim b) = a < b

infix 4 .>
(.>) :: (d1 @~ d2) => Dim d1 -> Dim d2 -> Bool
(Dim a) .> (Dim b) = a > b

infix 4 .<=
(.<=) :: (d1 @~ d2) => Dim d1 -> Dim d2 -> Bool
(Dim a) .<= (Dim b) = a <= b

infix 4 .>=
(.>=) :: (d1 @~ d2) => Dim d1 -> Dim d2 -> Bool
(Dim a) .>= (Dim b) = a >= b

dimEq :: (d1 @~ d2) => Double -> Dim d1 -> Dim d2 -> Bool
dimEq epsilon (Dim a) (Dim b) = abs(a-b) < epsilon

dimNeq :: (d1 @~ d2) => Double -> Dim d1 -> Dim d2 -> Bool
dimNeq epsilon (Dim a) (Dim b) = abs(a-b) >= epsilon

-- provide useful abbreviations
dimSqr :: Dim a -> Dim (Normalize (a @+ a))
dimSqr x = x .* x

dimSqrt :: Dim a -> Dim (a @/ Two)
dimSqrt = nthRoot pTwo

dimCubeRoot :: Dim a -> Dim (a @/ Three)
dimCubeRoot = nthRoot pThree

-- Scalar multiplication
infixl 7 *.
(*.) :: Double -> Dim a -> Dim a
a *. (Dim b) = Dim (a * b)

-------------------------------------------------------------
--- Instances -----------------------------------------------
-------------------------------------------------------------

deriving instance Eq (Dim dimspec)
deriving instance Ord (Dim dimspec)
deriving instance Num (Dim '[])
deriving instance Real (Dim '[])
deriving instance Fractional (Dim '[])
deriving instance Floating (Dim '[])
deriving instance RealFrac (Dim '[])
deriving instance RealFloat (Dim '[])

-------------------------------------------------------------
--- Combinators ---------------------------------------------
-------------------------------------------------------------

infixl 7 %*
type family (d1 :: *) %* (d2 :: *) :: *
type instance (Dim d1) %* (Dim d2) = Dim (d1 @+ d2)

infixl 7 %/
type family (d1 :: *) %/ (d2 :: *) :: *
type instance (Dim d1) %/ (Dim d2) = Dim (d1 @- d2)

infixr 8 %^
type family (d :: *) %^ (z :: Z) :: *
type instance (Dim d) %^ z = Dim (d @* z)


