{- Data/Dimensions/DimSpec.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file defines the DimSpec kind and operations over lists of DimSpecs
-}

{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances #-}

module Data.Dimensions.DimSpec where

import GHC.Exts (Constraint)
import Data.Dimensions.TypePrelude
import Data.Dimensions.Z

-- This will only be used at the kind level.
-- It either holds a dimension with its exponent, or the special constant DAny,
-- which can be any combination of dimensions at any exponents. It is used to
-- represent multiplying by 0 somewhere.
data DimSpec star = D star Z | DAny

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

-- Do these DimSpecs represent the same dimension?
infix 4 $=
type family (a :: DimSpec *) $= (b :: DimSpec *) :: Bool where
  (D n1 z1) $= (D n2 z2) = n1 :=: n2
  DAny      $= DAny      = True
  a         $= b         = False

-- Extract pulls the DimSpec that matches s out of lst, returning a diminished
-- list and, possibly, the extracted DimSpec.
--
-- Extract A [A, B, C] ==> ([B, C], Just A)
-- Extract D [A, B, C] ==> ([A, B, C], Nothing)
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

-- Reorders a to be the in the same order as b, putting entries not in b at the end
--
-- Reorder [A 1, B 2] [B 5, A 2] ==> [B 2, A 1]
-- Reorder [A 1, B 2, C 3] [C 2, A 8] ==> [C 3, A 1, B 2]
-- Reorder [A 1, B 2] [B 4, C 1, A 9] ==> [B 2, A 1]
-- Reorder x x ==> x
-- Reorder x [] ==> x
-- Reorder [] x ==> []
type family Reorder (a :: [DimSpec *]) (b :: [DimSpec *]) :: [DimSpec *] where
  Reorder x '[] = x
  Reorder x (h ': t) = Reorder' (Extract h x) t

type family Reorder' (scrut :: ([DimSpec *], Maybe (DimSpec *)))
                     (t :: [DimSpec *])
                     :: [DimSpec *] where
  Reorder' '(lst, Nothing) t = Reorder lst t
  Reorder' '(lst, Just elt) t = elt ': (Reorder lst t)

-- Check if a [DimSpec *] has a DAny inside it
type family HasAny (lst :: [DimSpec *]) :: Bool where
  HasAny '[]         = False
  HasAny (DAny ': t) = True
  HasAny (h ': t)    = HasAny t

-- Check if two [DimSpec *]s should be considered to be equal
infix 4 @~
type family (a :: [DimSpec *]) @~ (b :: [DimSpec *]) :: Constraint where
  a @~ b = If (HasAny a :||: HasAny b)
              (() :: Constraint)
              (Normalize (Reorder a b) ~ Normalize b)

----------------------------------------------------------
--- Normalization ----------------------------------------
----------------------------------------------------------

-- Take a [DimSpec *] and remove any DimSpecs with an exponent of 0
type family Normalize' (d :: [DimSpec *]) :: [DimSpec *] where
  Normalize' '[] = '[]
  Normalize' ((D n Zero) ': t) = Normalize' t
  Normalize' (h ': t) = h ': Normalize' t

-- If a [DimSpec *] has a DAny, collapse the whole list to one DAny. Otherwise,
-- normalize the list by removing exponents of 0.
type family Normalize (d :: [DimSpec *]) :: [DimSpec *] where
  Normalize d = If (HasAny d) '[DAny] (Normalize' d)

-- Given two [DimSpec *]s, return the one that lacks a DAny, if there is one.
type family ChooseFrom (d1 :: [DimSpec *]) (d2 :: [DimSpec *]) :: [DimSpec *] where
  ChooseFrom d d        = Normalize d
  ChooseFrom '[DAny] d2 = Normalize d2  -- common cases
  ChooseFrom d1 '[DAny] = Normalize d1
  ChooseFrom d1 d2      = Normalize (If (HasAny d1) d2 d1)

----------------------------------------------------------
--- Arithmetic -------------------------------------------
----------------------------------------------------------

-- Adds corresponding exponents in a dimension
infixl 6 @@+
type family (a :: [DimSpec *]) @@+ (b :: [DimSpec *]) :: [DimSpec *] where
  '[]                 @@+ b                   = b
  a                   @@+ '[]                 = a
  (DAny ': t1)        @@+ b                   = '[DAny]
  a                   @@+ (DAny ': t2)        = '[DAny]
  ((D name z1) ': t1) @@+ ((D name z2) ': t2) = (D name (z1 #+ z2)) ': (t1 @@+ t2)
  a                   @@+ (h ': t)            = h ': (a @@+ t)

-- Adds corresponding exponents in a dimension
infixl 6 @+
type family (a :: [DimSpec *]) @+ (b :: [DimSpec *]) :: [DimSpec *] where
  a @+ b = (Reorder a b) @@+ b

-- Subtract exponents in two dimensions
infixl 6 @@-
type family (a :: [DimSpec *]) @@- (b :: [DimSpec *]) :: [DimSpec *] where
  '[]                 @@- b                   = NegList b
  a                   @@- '[]                 = a
  (DAny ': t1)        @@- b                   = '[DAny]
  a                   @@- (DAny ': t2)        = '[DAny]
  ((D name z1) ': t1) @@- ((D name z2) ': t2) = (D name (z1 #- z2)) ': (t1 @@- t2)
  a                   @@- (h ': t)            = (NegDim h) ': (a @@- t)

-- Subtract exponents
infixl 6 @-
type family (a :: [DimSpec *]) @- (b :: [DimSpec *]) :: [DimSpec *] where
  a @- b = (Reorder a b) @@- b

-- negate a single DimSpec
type family NegDim (a :: DimSpec *) :: DimSpec * where
  NegDim (D n z) = D n (NegZ z)
  NegDim DAny    = DAny

-- negate a list of DimSpecs
type family NegList (a :: [DimSpec *]) :: [DimSpec *] where
  NegList '[]      = '[]
  NegList (h ': t) = (NegDim h ': (NegList t))

-- Multiplication
infixl 7 @*
type family (base :: [DimSpec *]) @* (power :: Z) :: [DimSpec *] where
  '[]                 @* power = '[]
  ((D name num) ': t) @* power = (D name (num #* power)) ': (t @* power)
  (DAny ': t)         @* power = DAny ': (t @* power)

-- Division
infixl 7 @/
type family (dims :: [DimSpec *]) @/ (z :: Z) :: [DimSpec *] where
  '[]                 @/ z = '[]
  ((D name num) ': t) @/ z = (D name (num #/ z)) ': (t @/ z)
  (DAny ': t)         @/ z = DAny ': (t @/ z)