-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.NoVector
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This file exports the same definitions as Data.Metrology, but it
-- restricts arithmetic operators to just use the Prelude numerical hierarchy,
-- instead of vector-space. It uses orphan instances for vector-space classes
-- to do this, so this module is not generally suitable for use in a released
-- library. On the other hand, using these definitions may be convenient for
-- quick experimentation, because the vector-space classes interfere with
-- GHC's ambiguity-resolution for overloaded numbers.
-----------------------------------------------------------------------------

{-# LANGUAGE TypeOperators, ConstraintKinds, DataKinds #-}

module Data.Metrology.NoVector (
  -- * Operators working with a default LCSU
  numIn, (#), quOf, (%), Count,

  -- * The rest of the @units@ package interface.

  -- | Though Haddock doesn't show it, the polymorphic versions of 'numIn',
  -- '#', 'quOf', '%', and 'Count' are not re-exported.
  module Data.Metrology.NoVector.Poly
  ) where

import Data.Metrology.NoVector.Poly hiding ( numIn, (#), quOf, (%), Count )
import Data.Metrology ( Count )
import qualified Data.Metrology.NoVector.Poly as Poly

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

infixr 9 %
-- | Infix synonym for 'quOf'
(%) :: ( ValidDLU dim DefaultLCSU unit
       , Fractional n )
    => n -> unit -> Qu dim DefaultLCSU n
(%) = quOf

