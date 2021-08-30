{- Data/Metrology/Qu.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   rae@cs.brynmawr.edu

   This file defines the 'Qu' type that represents quantity
   (a number paired with its measurement reference).
   This file also defines operations on 'Qu's that are shared between
   the vector and non-vector interfaces.
-}

{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds, UndecidableInstances,
             ConstraintKinds, StandaloneDeriving, GeneralizedNewtypeDeriving,
             FlexibleInstances, RoleAnnotations, FlexibleContexts,
             ScopedTypeVariables, CPP #-}

#if __GLASGOW_HASKELL__ >= 711
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

#if __GLASGOW_HASKELL__ >= 900
{-# OPTIONS_GHC -Wno-star-is-type #-}
#endif

module Data.Metrology.Qu where

import Data.Metrology.Dimensions
import Data.Metrology.Factor
import Data.Metrology.Units
import Data.Metrology.Z
import Data.Metrology.LCSU

import Control.DeepSeq (NFData (..))
import Data.VectorSpace

import Text.Read
import Data.Coerce

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

infixl 7 /|
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
deriving instance NFData n => NFData (Qu d l n)

deriving instance AdditiveGroup n => AdditiveGroup (Qu d l n)
instance VectorSpace n => VectorSpace (Qu d l n) where
  type Scalar (Qu d l n) = Scalar n
  a *^ (Qu b) = Qu (a *^ b)

-------------------------------------------------------------
--- Instances for dimensionless quantities ------------------
-------------------------------------------------------------

-- Express the condition on `d` via a constraint, so that the
-- requirement for the Num class can inform the choice of
-- dimension. See #35.
deriving instance (d ~ '[], Num n)        => Num (Qu d l n)
deriving instance (d ~ '[], Real n)       => Real (Qu d l n)
deriving instance (d ~ '[], Fractional n) => Fractional (Qu d l n)
deriving instance (d ~ '[], Floating n)   => Floating (Qu d l n)
deriving instance (d ~ '[], RealFrac n)   => RealFrac (Qu d l n)
deriving instance (d ~ '[], RealFloat n)  => RealFloat (Qu d l n)

-- But don't do this for Read and Show, because other instances
-- are indeed sensible. Using the above technique here would make
-- other instances impossible. Also, note that GeneralizedNewtypeDeriving
-- puts the "Qu" constructor in Read and Show instances, so don't use
-- that.
instance Show n => Show (Qu '[] l n) where
  showsPrec = coerce (showsPrec :: Int -> n -> ShowS)
  show      = coerce (show      :: n -> String)
  showList  = coerce (showList  :: [n] -> ShowS)

instance Read n => Read (Qu '[] l n) where
  readsPrec    = coerce (readsPrec    :: Int -> ReadS n)
  readList     = coerce (readList     :: ReadS [n])
  readPrec     = coerce (readPrec     :: ReadPrec n)
  readListPrec = coerce (readListPrec :: ReadPrec [n])

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

-------------------------------------------------------------
--- Term-level combinators ----------------------------------
-------------------------------------------------------------

-- | Use this to choose a default LCSU for a dimensioned quantity.
-- The default LCSU uses the 'DefaultUnitOfDim' representation for each
-- dimension.
defaultLCSU :: Qu dim DefaultLCSU n -> Qu dim DefaultLCSU n
defaultLCSU = id

-- | The number 1, expressed as a unitless dimensioned quantity.
unity :: Num n => Qu '[] l n
unity = Qu 1

-- | Cast between equivalent dimension within the same CSU.
--  for example [kg m s] and [s m kg]. See the README for more info.
redim :: (d @~ e) => Qu d l n -> Qu e l n
redim (Qu x) = Qu x

-- | The type of unitless dimensioned quantities.
-- This is an instance of @Num@, though Haddock doesn't show it.
-- This is parameterized by an LCSU and a number representation.
type Count = MkQu_ULN Number
