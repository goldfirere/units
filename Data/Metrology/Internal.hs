{- Data/Metrology/Internal.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file gathers and exports user-visible type-level definitions, to
   make error messages less cluttered. Non-expert users should never have
   to use the definitions exported from this module.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Internal
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This file gathers and exports user-visible type-level definitions, to make
-- error messages less cluttered. Non-expert users should never have to use
-- the definitions exported from this module.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExplicitNamespaces #-}

module Data.Metrology.Internal (
  -- * LCSU lookup
  Lookup, LookupList,

  -- * Validity checking
  ConvertibleLCSUs, ValidDLU, ValidDL, type (*~), CanonicalUnitsOfFactors,

  -- * Manipulating units
  DimOfUnitIsConsistent, IsCanonical,
  CanonicalUnit, CanonicalUnit', BaseHasConvRatio,
  UnitFactor, UnitFactorsOf,

  -- * Maniuplating dimension specifications
  module Data.Metrology.Factor,

  -- * Set operations on lists
  module Data.Metrology.Set,

  -- * Dimensions
  DimFactorsOf
  ) where

import Data.Metrology.LCSU
import Data.Metrology.Validity
import Data.Metrology.Units
import Data.Metrology.Factor
import Data.Metrology.Set
import Data.Metrology.Dimensions

  
