{-# LANGUAGE ConstraintKinds, TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.AltOperators
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines an alternative set of term level operators for
-- quantity calculus. These operators have nice, character-efficient
-- representation, and they are separated here and not in
-- "Data.Metrology" just to avoid current and future name conflict
-- with other libraries. Users not under such conflicts are free to
-- use these operators.
-----------------------------------------------------------------------------

module Data.Metrology.AltOperators where

import Data.Singletons (Sing)
import Data.Metrology.Factor
import Data.Metrology.Quantity
import Data.Metrology.Z

infixl 6 .+ , .-
-- | Add two compatible quantities
(.+) :: (d1 @~ d2, Num n) => Qu d1 l n -> Qu d2 l n -> Qu d1 l n
(.+) = (|+|)

-- | Subtract two compatible quantities
(.-) :: (d1 @~ d2, Num n) => Qu d1 l n -> Qu d2 l n -> Qu d1 l n
(.-) = (|-|)

infixl 7 .* , ./ , *.
-- | Multiply two quantities
(.*) :: Num n => Qu a l n -> Qu b l n -> Qu (Normalize (a @+ b)) l n
(.*) = (|*|)

-- | Multiply a quantity by a scalar
(*.) :: Num n => n -> Qu a l n -> Qu a l n
(*.) = (*|)

-- | Divide two quantities
(./) :: Fractional n => Qu a l n -> Qu b l n -> Qu (Normalize (a @- b)) l n
(./) = (|/|)

infixr 8 .^ , .^^
-- | Raise a quantity to a non-negative power known at compile time
(.^) :: Fractional n => Qu a l n -> Sing z -> Qu (a @* z) l n
(.^) = (|^)

-- | Raise a quantity to a power known at compile time
(.^^) :: Fractional n => Qu a l n -> Sing z -> Qu (a @* z) l n
(.^^) = (|^^)



infix 4 .< , .> , .<= , .>= , .== , ./=
-- | Check if one quantity is less than a compatible one
(.<) :: (d1 @~ d2, Ord n) => Qu d1 l n -> Qu d2 l n -> Bool
(.<) = (|<|)

-- | Check if one quantity is greater than a compatible one
(.>) :: (d1 @~ d2, Ord n) => Qu d1 l n -> Qu d2 l n -> Bool
(.>) = (|>|)

-- | Check if one quantity is less than or equal to a compatible one
(.<=) :: (d1 @~ d2, Ord n) => Qu d1 l n -> Qu d2 l n -> Bool
(.<=) = (|<=|)

-- | Check if one quantity is greater than or equal to a compatible one
(.>=) :: (d1 @~ d2, Ord n) => Qu d1 l n -> Qu d2 l n -> Bool
(.>=) = (|>=|)

(.==) :: (d1 @~ d2, Eq n) => Qu d1 l n -> Qu d2 l n -> Bool
(.==) = (|==|)

-- | Check if two quantities are not equal
(./=) :: (d1 @~ d2, Eq n) => Qu d1 l n -> Qu d2 l n -> Bool
(./=) = (|/=|)
