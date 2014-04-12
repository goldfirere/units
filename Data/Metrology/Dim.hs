{- Data/Metrology/Dim.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file defines the Dim type and operations on that type.
-}

{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds, UndecidableInstances,
             ConstraintKinds, StandaloneDeriving, GeneralizedNewtypeDeriving,
             FlexibleInstances, RoleAnnotations #-}

module Data.Metrology.Dim where

import Data.Singletons ( Sing )
import Data.Metrology.DimSpec
import Data.Metrology.Z
import Data.Metrology.LCSU

-------------------------------------------------------------
--- Internal ------------------------------------------------
-------------------------------------------------------------

-- | Dim adds a dimensional annotation to its base type @n@. This is the
-- representation for all dimensioned quantities.
newtype Dim (a :: [DimSpec *]) (lcsu :: LCSU *) (n :: *) = Dim n
type role Dim nominal nominal representational

-------------------------------------------------------------
--- User-facing ---------------------------------------------
-------------------------------------------------------------

infixl 6 .+
-- | Add two compatible dimensioned quantities
(.+) :: (d1 @~ d2, Num n) => Dim d1 l n -> Dim d2 l n -> Dim d1 l n
(Dim a) .+ (Dim b) = Dim (a + b)

infixl 6 .-
-- | Subtract two compatible dimensioned quantities
(.-) :: (d1 @~ d2, Num n) => Dim d1 l n -> Dim d2 l n -> Dim d1 l n
(Dim a) .- (Dim b) = Dim (a - b)

infixl 7 .*
-- | Multiply two dimensioned quantities
(.*) :: Num n => Dim a l n -> Dim b l n -> Dim (Normalize (a @+ b)) l n
(Dim a) .* (Dim b) = Dim (a * b)

infixl 7 ./
-- | Divide two dimensioned quantities
(./) :: Fractional n => Dim a l n -> Dim b l n -> Dim (Normalize (a @- b)) l n
(Dim a) ./ (Dim b) = Dim (a / b)

infixr 8 .^
-- | Raise a dimensioned quantity to a power known at compile time
(.^) :: Fractional n => Dim a l n -> Sing z -> Dim (a @* z) l n
(Dim a) .^ sz = Dim (a ^^ szToInt sz)

-- | Take the n'th root of a dimensioned quantity, where n is known at compile
-- time
nthRoot :: ((Zero < z) ~ True, Floating n)
        => Sing z -> Dim a l n -> Dim (a @/ z) l n
nthRoot sz (Dim a) = Dim (a ** (1.0 / (fromIntegral $ szToInt sz)))

infix 4 .<
-- | Check if one dimensioned quantity is less than a compatible one
(.<) :: (d1 @~ d2, Ord n) => Dim d1 l n -> Dim d2 l n -> Bool
(Dim a) .< (Dim b) = a < b

infix 4 .>
-- | Check if one dimensioned quantity is greater than a compatible one
(.>) :: (d1 @~ d2, Ord n) => Dim d1 l n -> Dim d2 l n -> Bool
(Dim a) .> (Dim b) = a > b

infix 4 .<=
-- | Check if one dimensioned quantity is less than or equal to a compatible one
(.<=) :: (d1 @~ d2, Ord n) => Dim d1 l n -> Dim d2 l n -> Bool
(Dim a) .<= (Dim b) = a <= b

infix 4 .>=
-- | Check if one dimensioned quantity is greater than or equal to a compatible one
(.>=) :: (d1 @~ d2, Ord n) => Dim d1 l n -> Dim d2 l n -> Bool
(Dim a) .>= (Dim b) = a >= b

-- | Compare two compatible dimensioned quantities for equality
dimEq :: (d0 @~ d1, d0 @~ d2, Num n, Ord n)
      => Dim d0 l n  -- ^ If the difference between the next
                     -- two arguments are less than this 
                     -- amount, they are considered equal
      -> Dim d1 l n -> Dim d2 l n -> Bool
dimEq (Dim epsilon) (Dim a) (Dim b) = abs(a-b) < epsilon

-- | Compare two compatible dimensioned quantities for inequality
dimNeq :: (d0 @~ d1, d0 @~ d2, Num n, Ord n)
       => Dim d0 l n -- ^ If the difference between the next
                     -- two arguments are less  than this 
                     -- amount, they are considered equal
       -> Dim d1 l n -> Dim d2 l n -> Bool
dimNeq (Dim epsilon) (Dim a) (Dim b) = abs(a-b) >= epsilon

-- | Square a dimensioned quantity
dimSqr :: Num n => Dim a l n -> Dim (Normalize (a @+ a)) l n
dimSqr x = x .* x

-- | Take the square root of a dimensioned quantity
dimSqrt :: Floating n => Dim a l n -> Dim (a @/ Two) l n
dimSqrt = nthRoot pTwo

-- | Take the cube root of a dimensioned quantity
dimCubeRoot :: Floating n => Dim a l n -> Dim (a @/ Three) l n
dimCubeRoot = nthRoot pThree

infixl 7 *.
-- | Multiply a dimensioned quantity by a scalar
(*.) :: Num n => n -> Dim a l n -> Dim a l n
a *. (Dim b) = Dim (a * b)

-------------------------------------------------------------
--- Instances -----------------------------------------------
-------------------------------------------------------------

deriving instance Eq n => Eq (Dim '[] l n)
deriving instance Ord n => Ord (Dim '[] l n)
deriving instance Num n => Num (Dim '[] l n)
deriving instance Real n => Real (Dim '[] l n)
deriving instance Fractional n => Fractional (Dim '[] l n)
deriving instance Floating n => Floating (Dim '[] l n)
deriving instance RealFrac n => RealFrac (Dim '[] l n)
deriving instance RealFloat n => RealFloat (Dim '[] l n)

-------------------------------------------------------------
--- Combinators ---------------------------------------------
-------------------------------------------------------------

infixl 7 %*
-- | Multiply two dimension types to produce a new one. For example:
--
-- > type Velocity = Length %/ Time
type family (d1 :: *) %* (d2 :: *) :: *
type instance (Dim d1 l n) %* (Dim d2 l n) = Dim (d1 @+ d2) l n

infixl 7 %/
-- | Divide two dimension types to produce a new one
type family (d1 :: *) %/ (d2 :: *) :: *
type instance (Dim d1 l n) %/ (Dim d2 l n) = Dim (d1 @- d2) l n

infixr 8 %^
-- | Exponentiate a dimension type to an integer
type family (d :: *) %^ (z :: Z) :: *
type instance (Dim d l n) %^ z = Dim (d @* z) l n


