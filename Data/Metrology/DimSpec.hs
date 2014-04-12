{- Data/Metrology.DimSpec.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file defines the DimSpec kind and operations over lists of DimSpecs.

   DimSpecs represents dimensions and units raised to a power of integers, and the lists of DimSpecs represents monomials of dimensions and units.
-}

{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances #-}

module Data.Metrology.DimSpec where

import GHC.Exts (Constraint)
import Data.Metrology.Z
import Data.Type.Equality
import Data.Type.Bool

import Data.Singletons.Tuple (Fst, Snd)

-- | This will only be used at the kind level. It holds a dimension with its
-- exponent.
data DimSpec star = D star Z

----------------------------------------------------------
--- Set-like operations ----------------------------------
----------------------------------------------------------
{-
These functions are templates for type-level functions.
remove :: String -> [String] -> [String]
remove _ [] = []
remove s (h:t) = if s == h then t else h : remove s t

member :: String -> [String] -> Bool
member _ [] = False
member s (h:t) = s == h || member s t

extract :: String -> [String] -> ([String], Maybe String)
extract _ [] = ([], Nothing)
extract s (h:t) =
  if s == h
   then (t, Just s)
   else let (resList, resVal) = extract s t in (h : resList, resVal)

reorder :: [String] -> [String] -> [String]
reorder x [] = x
reorder x (h:t) =
  case extract h x of
    (lst, Nothing) -> reorder lst t
    (lst, Just elt) -> elt : (reorder lst t)
-}

infix 4 $=
-- | Do these DimSpecs represent the same dimension?
type family (a :: DimSpec *) $= (b :: DimSpec *) :: Bool where
  (D n1 z1) $= (D n2 z2) = n1 == n2
  a         $= b         = False

-- | @(Extract s lst)@ pulls the DimSpec that matches s out of lst, returning a
--   diminished list and, possibly, the extracted DimSpec.
--
-- @
-- Extract A [A, B, C] ==> ([B, C], Just A
-- Extract D [A, B, C] ==> ([A, B, C], Nothing)
-- @
type family Extract (s :: DimSpec *)
                    (lst :: [DimSpec *])
                 :: ([DimSpec *], Maybe (DimSpec *)) where
  Extract s '[] = '( '[], Nothing )
  Extract s (h ': t) =
    If (s $= h)
      '(t, Just h)
      '(h ': Fst (Extract s t), Snd (Extract s t))

-- kind DimAnnotation = [DimSpec *]
-- a list of DimSpecs forms a full annotation of a quantity's dimension

-- | Reorders a to be the in the same order as b, putting entries not in b at the end
--
-- @
-- Reorder [A 1, B 2] [B 5, A 2] ==> [B 2, A 1]
-- Reorder [A 1, B 2, C 3] [C 2, A 8] ==> [C 3, A 1, B 2]
-- Reorder [A 1, B 2] [B 4, C 1, A 9] ==> [B 2, A 1]
-- Reorder x x ==> x
-- Reorder x [] ==> x
-- Reorder [] x ==> []
-- @
type family Reorder (a :: [DimSpec *]) (b :: [DimSpec *]) :: [DimSpec *] where
  Reorder x x = x
  Reorder x '[] = x
  Reorder x (h ': t) = Reorder' (Extract h x) t

-- | Helper function in 'Reorder'
type family Reorder' (scrut :: ([DimSpec *], Maybe (DimSpec *)))
                     (t :: [DimSpec *])
                     :: [DimSpec *] where
  Reorder' '(lst, Nothing) t = Reorder lst t
  Reorder' '(lst, Just elt) t = elt ': (Reorder lst t)

infix 4 @~
-- | Check if two @[DimSpec *]@s should be considered to be equal
type family (a :: [DimSpec *]) @~ (b :: [DimSpec *]) :: Constraint where
  a @~ b = (Normalize (Reorder a b) ~ Normalize b)

----------------------------------------------------------
--- Normalization ----------------------------------------
----------------------------------------------------------

-- | Take a @[DimSpec *]@ and remove any @DimSpec@s with an exponent of 0
type family Normalize (d :: [DimSpec *]) :: [DimSpec *] where
  Normalize '[] = '[]
  Normalize ((D n Zero) ': t) = Normalize t
  Normalize (h ': t) = h ': Normalize t

----------------------------------------------------------
--- Arithmetic -------------------------------------------
----------------------------------------------------------

infixl 6 @@+
-- | Adds corresponding exponents in two dimension, assuming the lists are
-- ordered similarly.
type family (a :: [DimSpec *]) @@+ (b :: [DimSpec *]) :: [DimSpec *] where
  '[]                 @@+ b                   = b
  a                   @@+ '[]                 = a
  ((D name z1) ': t1) @@+ ((D name z2) ': t2) = (D name (z1 #+ z2)) ': (t1 @@+ t2)
  a                   @@+ (h ': t)            = h ': (a @@+ t)

infixl 6 @+
-- | Adds corresponding exponents in two dimension
type family (a :: [DimSpec *]) @+ (b :: [DimSpec *]) :: [DimSpec *] where
  a @+ b = (Reorder a b) @@+ b

infixl 6 @@-
-- | Subtract exponents in two dimensions, assuming the lists are ordered
-- similarly.
type family (a :: [DimSpec *]) @@- (b :: [DimSpec *]) :: [DimSpec *] where
  '[]                 @@- b                   = NegList b
  a                   @@- '[]                 = a
  ((D name z1) ': t1) @@- ((D name z2) ': t2) = (D name (z1 #- z2)) ': (t1 @@- t2)
  a                   @@- (h ': t)            = (NegDim h) ': (a @@- t)

infixl 6 @-
-- | Subtract exponents in two dimensions
type family (a :: [DimSpec *]) @- (b :: [DimSpec *]) :: [DimSpec *] where
  a @- b = (Reorder a b) @@- b

-- | negate a single @DimSpec@
type family NegDim (a :: DimSpec *) :: DimSpec * where
  NegDim (D n z) = D n (NegZ z)

-- | negate a list of @DimSpec@s
type family NegList (a :: [DimSpec *]) :: [DimSpec *] where
  NegList '[]      = '[]
  NegList (h ': t) = (NegDim h ': (NegList t))

infixl 7 @*
-- | Multiplication of the exponents in a dimension by a scalar
type family (base :: [DimSpec *]) @* (power :: Z) :: [DimSpec *] where
  '[]                 @* power = '[]
  ((D name num) ': t) @* power = (D name (num #* power)) ': (t @* power)

infixl 7 @/
-- | Division of the exponents in a dimension by a scalar
type family (dims :: [DimSpec *]) @/ (z :: Z) :: [DimSpec *] where
  '[]                 @/ z = '[]
  ((D name num) ': t) @/ z = (D name (num #/ z)) ': (t @/ z)
