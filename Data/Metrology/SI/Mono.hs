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
-- with the DefaultLCSU. The difference between this module and
-- 'Data.Metrology.SI.MonoTypes' is that this module also exports
-- instances of 'DefaultUnitOfDim', necessary for use with
-- 'DefaultLCSU'.
-----------------------------------------------------------------------------

module Data.Metrology.SI.Mono (
  module Data.Metrology.SI.MonoTypes,
  module Data.Units.SI,
  module Data.Units.SI.Prefixes,
  module Data.Units.SI.Parser
  ) where

import Data.Metrology.SI.MonoTypes
import Data.Units.SI
import Data.Units.SI.Prefixes
import qualified Data.Dimensions.SI as D
import Data.Metrology
import Data.Units.SI.Parser

type instance DefaultUnitOfDim D.Length            = Meter
type instance DefaultUnitOfDim D.Mass              = Kilo :@ Gram
type instance DefaultUnitOfDim D.Time              = Second
type instance DefaultUnitOfDim D.Current           = Ampere
type instance DefaultUnitOfDim D.Temperature       = Kelvin
type instance DefaultUnitOfDim D.AmountOfSubstance = Mole
type instance DefaultUnitOfDim D.LuminousIntensity = Lumen
