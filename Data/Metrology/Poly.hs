{-# LANGUAGE ExplicitNamespaces #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Poly
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports all of the definitions you need if you wish to
-- write functions polymorphic over dimension specifications.
--
-- Each dimensioned quantity is represented by a member of the type
-- 'Qu', which is parameterized by a type-level list of 'DimSpec's and
-- a local coherent set of units ('LCSU').  A 'DimSpec', in turn, is a
-- dimension type paired with its exponent, representented with a
-- type-level integer 'Z'. Thus, the type of velocity in the SI system
-- would be @Qu '[D Meter One, D Second MOne]@.
--
-- A technical detail: because 'DimSpec' is used only at the type level
-- and needs to store types of kind @*@, it must be parameterized, as we
-- can't specify @*@ in its declaration. (See \"The Right Kind of Generic
-- Programming\", by José Pedro Magalhães, published at WGP'12, for more
-- explanation.) So, we always work with @(DimSpec *)@s.
----------------------------------------------------------------------------

module Data.Metrology.Poly (
  -- * The 'Dim' type
  Qu,

  -- * LCSUs (locally coherent system of units)
  LCSU, LookupList, Compatible,

  -- * Manipulating units and dimensions
  Dimension(..), Unit(..), DimOfUnitIsConsistent, IsCanonical,
  CanonicalUnit, type (*~),

  -- * Maniuplating dimension specifications
  DimSpec(..), type ($=), Extract, Reorder, type (@~), Normalize,
  type (@+), type (@-), NegDim, NegList, type (@*), type (@/),
  Canonicalize

  ) where

import Data.Metrology.Quantity
import Data.Metrology.Dimensions
import Data.Metrology.DimSpec
import Data.Metrology.LCSU
import Data.Metrology.Units
