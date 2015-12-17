{- Data/Metrology.hs

   The units Package
   Copyright (c) 2014 Richard Eisenberg
   eir@cis.upenn.edu

   This file gathers and exports all user-visible pieces of the units package.
   It also defines the main creators and consumers of dimensioned objects.

   This package declares many closely-related types. The following naming
   conventions should be helpful:

   Prefix  Target type/kind
   ------------------------
     #     Z
     $     Factor *
     @     [Factor *]
     @@    [Factor *], where the arguments are ordered similarly
     %     Qu (at the type level)
     |     Qu (at the term level)
     :     units & dimensions, at both type and term levels
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The units package is a framework for strongly-typed dimensional analysis.
-- This haddock documentation is generally /not/ enough to be able to use this
-- package effectively. Please see the readme at
-- <https://github.com/goldfirere/units/blob/master/README.md>.
--
-- Some of the types below refer to declarations that are not exported and
-- not documented here. This is because Haddock does not allow finely-tuned
-- abstraction in documentation. (In particular, right-hand sides of type 
-- synonym declarations are always included.) If a symbol is not exported,
-- you do /not/ need to know anything about it to use this package.
--
-- Though it doesn't appear here, @Count@ is an instance of @Num@, and
-- generally has all the numeric instances that @Double@ has.
--
-- This module exports definitions that lack unit-polymorphism. If you wish
-- to write more polymorphic code, see 'Data.Metrology.Poly'. If you wish
-- to use the numerical hierarchy from the @vector-space@ package, see
-- 'Data.Metrology.Vector'.
-----------------------------------------------------------------------------

{-# LANGUAGE TypeOperators, ConstraintKinds, DataKinds #-}

module Data.Metrology (
  -- * Operators working with a default LCSU
  numIn, (#), quOf, (%), Count,

  -- * The rest of the @units@ package interface.

  -- | Though Haddock doesn't show it, the polymorphic versions of 'numIn',
  -- '#', 'quOf', '%', and 'Count' are not re-exported.
  module Data.Metrology.Poly
  ) where

import Data.Metrology.Poly hiding ( numIn, (#), quOf, (%), Count )
import qualified Data.Metrology.Poly as Poly

-- | Extracts a numerical value from a dimensioned quantity, expressed in
--   the given unit. For example:
--
--   > inMeters :: Length -> Double
--   > inMeters x = numIn x Meter
--
--   or
--
--   > inMeters x = x # Meter   
numIn :: ( ValidDLU dim DefaultLCSU unit
         , Fractional n )
      => Qu dim DefaultLCSU n -> unit -> n
numIn = Poly.numIn

infix 5 #
-- | Infix synonym for 'numIn'
(#) :: ( ValidDLU dim DefaultLCSU unit
       , Fractional n )
    => Qu dim DefaultLCSU n -> unit -> n
(#) = numIn

-- | Creates a dimensioned quantity in the given unit. For example:
--
--   > height :: Length
--   > height = quOf 2.0 Meter
--
--   or
--
--   > height = 2.0 % Meter
quOf :: ( ValidDLU dim DefaultLCSU unit
        , Fractional n )
      => n -> unit -> Qu dim DefaultLCSU n
quOf = Poly.quOf

infixr 5 %
-- | Infix synonym for 'quOf'
(%) :: ( ValidDLU dim DefaultLCSU unit
       , Fractional n )
    => n -> unit -> Qu dim DefaultLCSU n
(%) = quOf

-- | The type of unitless dimensioned quantities.
-- This is an instance of @Num@, though Haddock doesn't show it.
-- This assumes a default LCSU and an internal representation of @Double@.
type Count = MkQu_U Number
