{- Data/Metrology.Factor.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file defines the Factor kind and operations over lists of Factors.

   Factors represents dimensions and units raised to a power of integers, and the lists of Factors represents monomials of dimensions and units.
-}

{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances #-}

module Data.Metrology.Factor where

import GHC.Exts (Constraint)
import Data.Metrology.Z
import Data.Type.Equality
import Data.Type.Bool

import Data.Singletons.Prelude

-- | This will only be used at the kind level. It holds a dimension or unit
-- with its exponent.
data Factor star = F star Z

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
-- | Do these Factors represent the same dimension?
type family (a :: Factor *) $= (b :: Factor *) :: Bool where
  (F n1 z1) $= (F n2 z2) = n1 == n2
  a         $= b         = False

-- | @(Extract s lst)@ pulls the Factor that matches s out of lst, returning a
--   diminished list and, possibly, the extracted Factor.
--
-- @
-- Extract A [A, B, C] ==> ([B, C], Just A
-- Extract F [A, B, C] ==> ([A, B, C], Nothing)
-- @
type family Extract (s :: Factor *)
                    (lst :: [Factor *])
                 :: ([Factor *], Maybe (Factor *)) where
  Extract s '[] = '( '[], Nothing )
  Extract s (h ': t) =
    If (s $= h)
      '(t, Just h)
      '(h ': Fst (Extract s t), Snd (Extract s t))

-- kind DimAnnotation = [Factor *]
-- a list of Factors forms a full annotation of a quantity's dimension

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
type family Reorder (a :: [Factor *]) (b :: [Factor *]) :: [Factor *] where
  Reorder x x = x
  Reorder '[] x = '[]
  Reorder x '[] = x
  Reorder x (h ': t) = Reorder' (Extract h x) t

-- | Helper function in 'Reorder'
type family Reorder' (scrut :: ([Factor *], Maybe (Factor *)))
                     (t :: [Factor *])
                     :: [Factor *] where
  Reorder' '(lst, Nothing) t = Reorder lst t
  Reorder' '(lst, Just elt) t = elt ': (Reorder lst t)

infix 4 @~
-- | Check if two @[Factor *]@s should be considered to be equal
type family (a :: [Factor *]) @~ (b :: [Factor *]) :: Constraint where
  a @~ b = (Normalize (Reorder a b) ~ Normalize b)

----------------------------------------------------------
--- Normalization ----------------------------------------
----------------------------------------------------------

-- | Take a @[Factor *]@ and remove any @Factor@s with an exponent of 0
type family Normalize (d :: [Factor *]) :: [Factor *] where
  Normalize '[] = '[]
  Normalize ((F n Zero) ': t) = Normalize t
  Normalize (h ': t) = h ': Normalize t

----------------------------------------------------------
--- Arithmetic -------------------------------------------
----------------------------------------------------------

infixl 6 @@+
-- | Adds corresponding exponents in two dimension, assuming the lists are
-- ordered similarly.
type family (a :: [Factor *]) @@+ (b :: [Factor *]) :: [Factor *] where
  '[]                 @@+ b                   = b
  a                   @@+ '[]                 = a
  ((F name z1) ': t1) @@+ ((F name z2) ': t2) = (F name (z1 #+ z2)) ': (t1 @@+ t2)
  a                   @@+ (h ': t)            = h ': (a @@+ t)

infixl 6 @+
-- | Adds corresponding exponents in two dimension
type family (a :: [Factor *]) @+ (b :: [Factor *]) :: [Factor *] where
  a @+ b = (Reorder a b) @@+ b

infixl 6 @@-
-- | Subtract exponents in two dimensions, assuming the lists are ordered
-- similarly.
type family (a :: [Factor *]) @@- (b :: [Factor *]) :: [Factor *] where
  '[]                 @@- b                   = NegList b
  a                   @@- '[]                 = a
  ((F name z1) ': t1) @@- ((F name z2) ': t2) = (F name (z1 #- z2)) ': (t1 @@- t2)
  a                   @@- (h ': t)            = (NegDim h) ': (a @@- t)

infixl 6 @-
-- | Subtract exponents in two dimensions
type family (a :: [Factor *]) @- (b :: [Factor *]) :: [Factor *] where
  a @- b = (Reorder a b) @@- b

-- | negate a single @Factor@
type family NegDim (a :: Factor *) :: Factor * where
  NegDim (F n z) = F n (NegZ z)

-- | negate a list of @Factor@s
type family NegList (a :: [Factor *]) :: [Factor *] where
  NegList '[]      = '[]
  NegList (h ': t) = (NegDim h ': (NegList t))

infixl 7 @*
-- | Multiplication of the exponents in a dimension by a scalar
type family (base :: [Factor *]) @* (power :: Z) :: [Factor *] where
  '[]                 @* power = '[]
  ((F name num) ': t) @* power = (F name (num #* power)) ': (t @* power)

infixl 7 @/
-- | Division of the exponents in a dimension by a scalar
type family (dims :: [Factor *]) @/ (z :: Z) :: [Factor *] where
  '[]                 @/ z = '[]
  ((F name num) ': t) @/ z = (F name (num #/ z)) ': (t @/ z)
