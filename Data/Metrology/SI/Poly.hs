{-# LANGUAGE TypeOperators, DataKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.SI.Poly
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports definitions for the SI system, with the intent
-- of using these definitions in a polymorphic manner -- that is,
-- with multiple LCSUs.
-----------------------------------------------------------------------------

module Data.Metrology.SI.Poly (
  SI,
  module Data.Metrology.SI.PolyTypes,
  module Data.Metrology.SI.Prefixes,
  module Data.Metrology.SI.Units
  ) where

import Data.Metrology.SI.PolyTypes
import Data.Metrology.SI.Prefixes
import Data.Metrology.SI.Units
import qualified Data.Metrology.SI.Dims as D
import Data.Metrology

type SI = MkLCSU '[ (D.Length, Meter)
                  , (D.Mass, Kilo :@ Gram)
                  , (D.Time, Second)
                  , (D.Current, Ampere)
                  , (D.Temperature, Kelvin)
                  , (D.AmountOfSubstance, Mole)
                  , (D.LuminousIntensity, Lumen)
                  ]
