{- Data/Metrology.Quantity.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file defines the 'Qu' type that represents quantity
   (a number paired with its measurement reference).
   This file also defines operations on 'Qu' types.
-}

{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds, UndecidableInstances,
             ConstraintKinds, StandaloneDeriving, GeneralizedNewtypeDeriving,
             FlexibleInstances, RoleAnnotations #-}

module Data.Metrology.Quantity where

import Data.Singletons ( Sing )
import Data.Metrology.Dimensions
import Data.Metrology.Factor
import Data.Metrology.Units
import Data.Metrology.Z
import Data.Metrology.LCSU

-------------------------------------------------------------
--- Internal ------------------------------------------------
-------------------------------------------------------------

-- | 'Qu' adds a dimensional annotation to its numerical value type
-- @n@. This is the representation for all quantities.
newtype Qu (a :: [Factor *]) (lcsu :: LCSU *) (n :: *) = Qu n
type role Qu nominal nominal representational

-------------------------------------------------------------
--- User-facing ---------------------------------------------
-------------------------------------------------------------

-- Abbreviation for creating a Qu (defined here to avoid a module cycle)

-- | Make a quantity type capable of storing a value of a given
-- unit. This uses a 'Double' for storage of the value. For example:
--
-- > data LengthDim = LengthDim
-- > instance Dimension LengthDim
-- > type Length = MkQu LengthDim
type MkQu dim = Qu (DimFactorsOf dim) DefaultLCSU Double

-- | Make a quantity type with a custom numerical type and LCSU.
type MkGenQu dim = Qu (DimFactorsOf dim)


infixl 6 .+
-- | Add two compatible quantities
(.+) :: (d1 @~ d2, Num n) => Qu d1 l n -> Qu d2 l n -> Qu d1 l n
(Qu a) .+ (Qu b) = Qu (a + b)

infixl 6 .-
-- | Subtract two compatible quantities
(.-) :: (d1 @~ d2, Num n) => Qu d1 l n -> Qu d2 l n -> Qu d1 l n
(Qu a) .- (Qu b) = Qu (a - b)

infixl 7 .*
-- | Multiply two quantities
(.*) :: Num n => Qu a l n -> Qu b l n -> Qu (Normalize (a @+ b)) l n
(Qu a) .* (Qu b) = Qu (a * b)

infixl 7 ./
-- | Divide two quantities
(./) :: Fractional n => Qu a l n -> Qu b l n -> Qu (Normalize (a @- b)) l n
(Qu a) ./ (Qu b) = Qu (a / b)

infixr 8 .^
-- | Raise a quantity to a power known at compile time
(.^) :: Fractional n => Qu a l n -> Sing z -> Qu (a @* z) l n
(Qu a) .^ sz = Qu (a ^^ szToInt sz)

-- | Take the n'th root of a quantity, where n is known at compile
-- time
nthRoot :: ((Zero < z) ~ True, Floating n)
        => Sing z -> Qu a l n -> Qu (a @/ z) l n
nthRoot sz (Qu a) = Qu (a ** (1.0 / (fromIntegral $ szToInt sz)))

infix 4 .<
-- | Check if one quantity is less than a compatible one
(.<) :: (d1 @~ d2, Ord n) => Qu d1 l n -> Qu d2 l n -> Bool
(Qu a) .< (Qu b) = a < b

infix 4 .>
-- | Check if one quantity is greater than a compatible one
(.>) :: (d1 @~ d2, Ord n) => Qu d1 l n -> Qu d2 l n -> Bool
(Qu a) .> (Qu b) = a > b

infix 4 .<=
-- | Check if one quantity is less than or equal to a compatible one
(.<=) :: (d1 @~ d2, Ord n) => Qu d1 l n -> Qu d2 l n -> Bool
(Qu a) .<= (Qu b) = a <= b

infix 4 .>=
-- | Check if one quantity is greater than or equal to a compatible one
(.>=) :: (d1 @~ d2, Ord n) => Qu d1 l n -> Qu d2 l n -> Bool
(Qu a) .>= (Qu b) = a >= b

-- | Compare two compatible quantities for equality
dimEq :: (d0 @~ d1, d0 @~ d2, Num n, Ord n)
      => Qu d0 l n  -- ^ If the difference between the next
                     -- two arguments are less than this 
                     -- amount, they are considered equal
      -> Qu d1 l n -> Qu d2 l n -> Bool
dimEq (Qu epsilon) (Qu a) (Qu b) = abs(a-b) < epsilon

-- | Compare two compatible quantities for inequality
dimNeq :: (d0 @~ d1, d0 @~ d2, Num n, Ord n)
       => Qu d0 l n -- ^ If the difference between the next
                     -- two arguments are less  than this 
                     -- amount, they are considered equal
       -> Qu d1 l n -> Qu d2 l n -> Bool
dimNeq (Qu epsilon) (Qu a) (Qu b) = abs(a-b) >= epsilon

-- | Square a quantity
dimSqr :: Num n => Qu a l n -> Qu (Normalize (a @+ a)) l n
dimSqr x = x .* x

-- | Take the square root of a quantity
dimSqrt :: Floating n => Qu a l n -> Qu (a @/ Two) l n
dimSqrt = nthRoot pTwo

-- | Take the cube root of a quantity
dimCubeRoot :: Floating n => Qu a l n -> Qu (a @/ Three) l n
dimCubeRoot = nthRoot pThree

infixl 7 *.
-- | Multiply a quantity by a scalar
(*.) :: Num n => n -> Qu a l n -> Qu a l n
a *. (Qu b) = Qu (a * b)

-------------------------------------------------------------
--- Instances -----------------------------------------------
-------------------------------------------------------------

deriving instance Eq n => Eq (Qu '[] l n)
deriving instance Ord n => Ord (Qu '[] l n)
deriving instance Num n => Num (Qu '[] l n)
deriving instance Real n => Real (Qu '[] l n)
deriving instance Fractional n => Fractional (Qu '[] l n)
deriving instance Floating n => Floating (Qu '[] l n)
deriving instance RealFrac n => RealFrac (Qu '[] l n)
deriving instance RealFloat n => RealFloat (Qu '[] l n)

-------------------------------------------------------------
--- Combinators ---------------------------------------------
-------------------------------------------------------------

infixl 7 %*
-- | Multiply two dimension types to produce a new one. For example:
--
-- > type Velocity = Length %/ Time
type family (d1 :: *) %* (d2 :: *) :: *
type instance (Qu d1 l n) %* (Qu d2 l n) = Qu (d1 @+ d2) l n

infixl 7 %/
-- | Divide two dimension types to produce a new one
type family (d1 :: *) %/ (d2 :: *) :: *
type instance (Qu d1 l n) %/ (Qu d2 l n) = Qu (d1 @- d2) l n

infixr 8 %^
-- | Exponentiate a dimension type to an integer
type family (d :: *) %^ (z :: Z) :: *
type instance (Qu d l n) %^ z = Qu (d @* z) l n


