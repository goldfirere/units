{- Data/Metrology/Qu.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file defines the 'Qu' type that represents quantity
   (a number paired with its measurement reference).
   This file also defines operations on 'Qu' types.
-}

{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds, UndecidableInstances,
             ConstraintKinds, StandaloneDeriving, GeneralizedNewtypeDeriving,
             FlexibleInstances, RoleAnnotations, FlexibleContexts #-}

module Data.Metrology.Qu where

import Data.Metrology.Dimensions
import Data.Metrology.Factor
import Data.Metrology.Units
import Data.Metrology.Z
import Data.Metrology.LCSU

import Data.AdditiveGroup
import Data.VectorSpace

import Data.Foldable as F

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
-- > data Meter = Meter
-- > instance Unit Meter where
-- >   type BaseUnit Meter = Canonical
-- >   type DimOfUnit Meter = LengthDim
-- > type instance DefaultUnitOfDim LengthDim = Meter
-- > type Length = MkQu_D LengthDim
--
-- Note that the dimension /must/ have an instance for the type family
-- 'DefaultUnitOfDim' for this to work.
type MkQu_D dim = Qu (DimFactorsOf dim) DefaultLCSU Double

-- | Make a quantity type with a custom numerical type and LCSU.
type MkQu_DLN dim = Qu (DimFactorsOf dim)

-- | Make a quantity type with a given unit. It will be stored as a 'Double'.
-- Note that the corresponding dimension /must/ have an appropriate instance
-- for 'DefaultUnitOfDim' for this to work.
type MkQu_U unit = Qu (DimFactorsOf (DimOfUnit unit)) DefaultLCSU Double

-- | Make a quantity type with a unit and LCSU with custom numerical type.
--   The quantity will have the dimension corresponding to the unit.
type MkQu_ULN unit = Qu (DimFactorsOf (DimOfUnit unit))

---------------------------------------
---------------------------------------
-- Privileged operations
---------------------------------------
---------------------------------------

---------------------------------------
-- Quantities of dimension one
---------------------------------------

-- | Convert a raw number into a unitless dimensioned quantity
quantity :: n -> Qu '[] l n
quantity = Qu

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

---------------------------------------
-- Multiplicative operations
---------------------------------------

infixl 7 |*|
-- | Multiply two quantities
(|*|) :: Num n => Qu a l n -> Qu b l n -> Qu (Normalize (a @+ b)) l n
(Qu a) |*| (Qu b) = Qu (a * b)

infixl 7 |/|
-- | Divide two quantities
(|/|) :: Fractional n => Qu a l n -> Qu b l n -> Qu (Normalize (a @- b)) l n
(Qu a) |/| (Qu b) = Qu (a / b)

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

-- | Divide a quantity by a scalar
(|/) :: (VectorSpace n, Fractional (Scalar n)) => Qu a l n -> Scalar n -> Qu (Normalize a) l n
(Qu a) |/ b = Qu (a ^/ b)
-- The above function should *not* need to be privileged. But, GHC can't figure
-- out that a @@- '[] ~ a. Urgh.

---------------------------------------
-- Exponentiation
---------------------------------------

-- The following are privileged for efficiency.

infixr 8 |^
-- | Raise a quantity to a integer power, knowing at compile time that the integer is non-negative.
(|^) :: (NonNegative z, Num n) => Qu a l n -> Sing z -> Qu (a @* z) l n
(Qu a) |^ sz = Qu (a ^ szToInt sz)

infixr 8 |^^
-- | Raise a quantity to a integer power known at compile time
(|^^) :: Fractional n => Qu a l n -> Sing z -> Qu (a @* z) l n
(Qu a) |^^ sz = Qu (a ^^ szToInt sz)

-- | Take the n'th root of a quantity, where n is known at compile
-- time
qNthRoot :: ((Zero < z) ~ True, Floating n)
        => Sing z -> Qu a l n -> Qu (a @/ z) l n
qNthRoot sz (Qu a) = Qu (a ** (1.0 / (fromIntegral $ szToInt sz)))

---------------------------------------
-- Comparison
---------------------------------------

-- | Compare two quantities
qCompare :: (d1 @~ d2, Ord n) => Qu d1 l n -> Qu d2 l n -> Ordering
qCompare (Qu a) (Qu b) = compare a b

infix 4 |<|
-- | Check if one quantity is less than a compatible one
(|<|) :: (d1 @~ d2, Ord n) => Qu d1 l n -> Qu d2 l n -> Bool
(Qu a) |<| (Qu b) = a < b

infix 4 |>|
-- | Check if one quantity is greater than a compatible one
(|>|) :: (d1 @~ d2, Ord n) => Qu d1 l n -> Qu d2 l n -> Bool
(Qu a) |>| (Qu b) = a > b

infix 4 |<=|
-- | Check if one quantity is less than or equal to a compatible one
(|<=|) :: (d1 @~ d2, Ord n) => Qu d1 l n -> Qu d2 l n -> Bool
(Qu a) |<=| (Qu b) = a <= b

infix 4 |>=|
-- | Check if one quantity is greater than or equal to a compatible one
(|>=|) :: (d1 @~ d2, Ord n) => Qu d1 l n -> Qu d2 l n -> Bool
(Qu a) |>=| (Qu b) = a >= b

infix 4 |==|
-- | Check if two quantities are equal (uses the equality of the underlying numerical type)
(|==|) :: (d1 @~ d2, Eq n) => Qu d1 l n -> Qu d2 l n -> Bool
(Qu a) |==| (Qu b) = a == b

infix 4 |/=|
-- | Check if two quantities are not equal
(|/=|) :: (d1 @~ d2, Eq n) => Qu d1 l n -> Qu d2 l n -> Bool
(Qu a) |/=| (Qu b) = a /= b

infix 4 `qApprox` , `qNapprox`
-- | Compare two compatible quantities for approximate equality. If the
-- difference between the left hand side and the right hand side arguments are
-- less than or equal to the /epsilon/, they are considered equal.
qApprox :: (d0 @~ d1, d0 @~ d2, Num n, Ord n)
      => Qu d0 l n  -- ^ /epsilon/
      -> Qu d1 l n  -- ^ left hand side
      -> Qu d2 l n  -- ^ right hand side
      -> Bool  
qApprox (Qu epsilon) (Qu a) (Qu b) = abs(a-b) <= epsilon

-- | Compare two compatible quantities for approixmate inequality.  
-- @qNapprox e a b = not $ qApprox e a b@
qNapprox :: (d0 @~ d1, d0 @~ d2, Num n, Ord n)
       => Qu d0 l n  -- ^ /epsilon/      
       -> Qu d1 l n  -- ^ left hand side 
       -> Qu d2 l n  -- ^ right hand side
       -> Bool
qNapprox (Qu epsilon) (Qu a) (Qu b) = abs(a-b) > epsilon

---------------------------------------
---------------------------------------
-- Unprivileged operations
---------------------------------------
---------------------------------------

infixl 6 |-|
-- | Subtract two compatible quantities
(|-|) :: (d1 @~ d2, AdditiveGroup n) => Qu d1 l n -> Qu d2 l n -> Qu d1 l n
a |-| b = a |+| qNegate b

-- | Take the sum of a list of quantities
qSum :: (Foldable f, AdditiveGroup n) => f (Qu d l n) -> Qu d l n
qSum = F.foldr (|+|) zero

infixl 7 *| , |* , /| , |/
-- | Multiply a quantity by a scalar from the left
(*|) :: VectorSpace n => Scalar n -> Qu b l n -> Qu (Normalize b) l n
a *| b = quantity a |*^| b

-- | Multiply a quantity by a scalar from the right
(|*) :: VectorSpace n => Qu a l n -> Scalar n -> Qu (Normalize a) l n
a |* b = a |^*| quantity b

-- | Divide a scalar by a quantity
(/|) :: Fractional n => n -> Qu b l n -> Qu (Normalize ('[] @- b)) l n
a /| b = quantity a |/| b

-- | Square a quantity
qSq :: Num n => Qu a l n -> Qu (Normalize (a @+ a)) l n
qSq x = x |*| x

-- | Cube a quantity
qCube :: Num n => Qu a l n -> Qu (Normalize (Normalize (a @+ a) @+ a)) l n
qCube x = x |*| x |*| x

-- | Take the square root of a quantity
qSqrt :: Floating n => Qu a l n -> Qu (a @/ Two) l n
qSqrt = qNthRoot sTwo

-- | Take the cubic root of a quantity
qCubeRoot :: Floating n => Qu a l n -> Qu (a @/ Three) l n
qCubeRoot = qNthRoot sThree

-------------------------------------------------------------
--- Instances for all quantities ----------------------------
-------------------------------------------------------------

deriving instance Eq n => Eq (Qu d l n)
deriving instance Ord n => Ord (Qu d l n)

-------------------------------------------------------------
--- Instances for dimensionless quantities ------------------
-------------------------------------------------------------

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
-- | Multiply two quantity types to produce a new one. For example:
--
-- > type Velocity = Length %/ Time
type family (d1 :: *) %* (d2 :: *) :: *
type instance (Qu d1 l n) %* (Qu d2 l n) = Qu (d1 @+ d2) l n

infixl 7 %/
-- | Divide two quantity types to produce a new one
type family (d1 :: *) %/ (d2 :: *) :: *
type instance (Qu d1 l n) %/ (Qu d2 l n) = Qu (d1 @- d2) l n

infixr 8 %^
-- | Exponentiate a quantity type to an integer
type family (d :: *) %^ (z :: Z) :: *
type instance (Qu d l n) %^ z = Qu (d @* z) l n


