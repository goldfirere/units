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

import Data.Singletons ( Sing )
import Data.Dimensions.DimSpec
import Data.Dimensions.Z

-------------------------------------------------------------
--- Internal ------------------------------------------------
-------------------------------------------------------------

-- | Dim adds a dimensional annotation to its base type @n@. This is the
-- representation for all dimensioned quantities.
newtype Dim (n :: *) (a :: [DimSpec *]) = Dim n

-------------------------------------------------------------
--- User-facing ---------------------------------------------
-------------------------------------------------------------

infixl 6 .+
-- | Add two compatible dimensioned quantities
(.+) :: (d1 @~ d2, Num n) => Dim n d1 -> Dim n d2 -> Dim n d1
(Dim a) .+ (Dim b) = Dim (a + b)

infixl 6 .-
-- | Subtract two compatible dimensioned quantities
(.-) :: (d1 @~ d2, Num n) => Dim n d1 -> Dim n d2 -> Dim n d1
(Dim a) .- (Dim b) = Dim (a - b)

infixl 7 .*
-- | Multiply two dimensioned quantities
(.*) :: Num n => Dim n a -> Dim n b -> Dim n (Normalize (a @+ b))
(Dim a) .* (Dim b) = Dim (a * b)

infixl 7 ./
-- | Divide two dimensioned quantities
(./) :: Fractional n => Dim n a -> Dim n b -> Dim n (Normalize (a @- b))
(Dim a) ./ (Dim b) = Dim (a / b)

infixr 8 .^
-- | Raise a dimensioned quantity to a power known at compile time
(.^) :: Fractional n => Dim n a -> Sing z -> Dim n (a @* z)
(Dim a) .^ sz = Dim (a ^^ szToInt sz)

-- | Take the n'th root of a dimensioned quantity, where n is known at compile
-- time
nthRoot :: ((Zero < z) ~ True, Floating n) => Sing z -> Dim n a -> Dim n (a @/ z)
nthRoot sz (Dim a) = Dim (a ** (1.0 / (fromIntegral $ szToInt sz)))

infix 4 .<
-- | Check if one dimensioned quantity is less than a compatible one
(.<) :: (d1 @~ d2, Ord n) => Dim n d1 -> Dim n d2 -> Bool
(Dim a) .< (Dim b) = a < b

infix 4 .>
-- | Check if one dimensioned quantity is greater than a compatible one
(.>) :: (d1 @~ d2, Ord n) => Dim n d1 -> Dim n d2 -> Bool
(Dim a) .> (Dim b) = a > b

infix 4 .<=
-- | Check if one dimensioned quantity is less than or equal to a compatible one
(.<=) :: (d1 @~ d2, Ord n) => Dim n d1 -> Dim n d2 -> Bool
(Dim a) .<= (Dim b) = a <= b

infix 4 .>=
-- | Check if one dimensioned quantity is greater than or equal to a compatible one
(.>=) :: (d1 @~ d2, Ord n) => Dim n d1 -> Dim n d2 -> Bool
(Dim a) .>= (Dim b) = a >= b

-- | Compare two compatible dimensioned quantities for equality
dimEq :: (d0 @~ d1, d0 @~ d2, Num n, Ord n)
      => Dim n d0  -- ^ If the difference between the next
                   -- two arguments are less  than this 
                   -- amount, they are considered equal
      -> Dim n d1 -> Dim n d2 -> Bool
dimEq (Dim epsilon) (Dim a) (Dim b) = abs(a-b) < epsilon

-- | Compare two compatible dimensioned quantities for inequality
dimNeq :: (d0 @~ d1, d0 @~ d2, Num n, Ord n)
       => Dim n d0 -- ^ If the difference between the next
                   -- two arguments are less  than this 
                   -- amount, they are considered equal
       -> Dim n d1 -> Dim n d2 -> Bool
dimNeq (Dim epsilon) (Dim a) (Dim b) = abs(a-b) >= epsilon

-- | Square a dimensioned quantity
dimSqr :: Num n => Dim n a -> Dim n (Normalize (a @+ a))
dimSqr x = x .* x

-- | Take the square root of a dimensioned quantity
dimSqrt :: Floating n => Dim n a -> Dim n (a @/ Two)
dimSqrt = nthRoot pTwo

-- | Take the cube root of a dimensioned quantity
dimCubeRoot :: Floating n => Dim n a -> Dim n (a @/ Three)
dimCubeRoot = nthRoot pThree

infixl 7 *.
-- | Multiply a dimensioned quantity by a scalar
(*.) :: Num n => n -> Dim n a -> Dim n a
a *. (Dim b) = Dim (a * b)

-------------------------------------------------------------
--- Instances -----------------------------------------------
-------------------------------------------------------------

deriving instance Eq n => Eq (Dim n '[])
deriving instance Ord n => Ord (Dim n '[])
deriving instance Num n => Num (Dim n '[])
deriving instance Real n => Real (Dim n '[])
deriving instance Fractional n => Fractional (Dim n '[])
deriving instance Floating n => Floating (Dim n '[])
deriving instance RealFrac n => RealFrac (Dim n '[])
deriving instance RealFloat n => RealFloat (Dim n '[])

-------------------------------------------------------------
--- Combinators ---------------------------------------------
-------------------------------------------------------------

infixl 7 %*
-- | Multiply two dimension types to produce a new one. For example:
--
-- > type Velocity = Length %/ Time
type family (d1 :: *) %* (d2 :: *) :: *
type instance (Dim n d1) %* (Dim n d2) = Dim n (d1 @+ d2)

infixl 7 %/
-- | Divide two dimension types to produce a new one
type family (d1 :: *) %/ (d2 :: *) :: *
type instance (Dim n d1) %/ (Dim n d2) = Dim n (d1 @- d2)

infixr 8 %^
-- | Exponentiate a dimension type to an integer
type family (d :: *) %^ (z :: Z) :: *
type instance (Dim n d) %^ z = Dim n (d @* z)


