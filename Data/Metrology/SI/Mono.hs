{-# LANGUAGE TypeOperators, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.SI.Mono
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports definitions for the SI system, with the intent
-- of using these definitions in a monomorphic manner -- that is,
-- with the DefaultLCSU.
-----------------------------------------------------------------------------

module Data.Metrology.SI.Mono (
  module Data.Metrology.SI.MonoTypes,
  module Data.Metrology.SI.Units,
  module Data.Metrology.SI.Prefixes,
  module Data.Metrology.SI.Parser
  ) where

import Data.Metrology.SI.MonoTypes
import Data.Metrology.SI.Units
import Data.Metrology.SI.Prefixes
import qualified Data.Metrology.SI.Dims as D
import Data.Metrology
import Data.Metrology.SI.Parser

type instance DefaultUnitOfDim D.Length            = Meter
type instance DefaultUnitOfDim D.Mass              = Kilo :@ Gram
type instance DefaultUnitOfDim D.Time              = Second
type instance DefaultUnitOfDim D.Current           = Ampere
type instance DefaultUnitOfDim D.Temperature       = Kelvin
type instance DefaultUnitOfDim D.AmountOfSubstance = Mole
type instance DefaultUnitOfDim D.LuminousIntensity = Lumen
