{-# LANGUAGE PatternSynonyms, TemplateHaskell, TypeOperators,
             TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.CGS
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines units used in the centimeter/gram/second system
-- of measurement.
--
-- Included are all mechanical units mentioned here:
-- http://en.wikipedia.org/wiki/Centimetre%E2%80%93gram%E2%80%93second_system_of_units
--
-- Some electromagnetic units are not included, because these do not have
-- reliable conversions to/from the SI units, on which the @units-defs@
-- edifice is based.
-----------------------------------------------------------------------------

module Data.Units.CGS (
  Centi(..), centi, Meter(..), pattern Metre, Gram(..), Second(..),
  module Data.Units.CGS
  ) where

import Data.Units.SI
import Data.Metrology.Poly
import Data.Metrology.TH

type Centimeter = Centi :@ Meter
pattern Centimeter = Centi :@ Meter

type Centimetre = Centimeter
pattern Centimetre = Centimeter

declareDerivedUnit "Gal"
  [t| Centimeter :/ Second :^ Two |]                1 (Just "Gal")
declareDerivedUnit "Dyne"
  [t| Gram :* Centimeter :/ Second :^ Two |]        1 (Just "dyn")
declareDerivedUnit "Erg"
  [t| Gram :* Centimeter :^ Two :/ Second :^ Two |] 1 (Just "erg")
declareDerivedUnit "Barye"
  [t| Gram :/ (Centimeter :* Second :^ Two) |]      1 (Just "Ba")
declareDerivedUnit "Poise"
  [t| Gram :/ (Centimeter :* Second) |]             1 (Just "P")
declareDerivedUnit "Stokes"
  [t| Centimeter :^ Two :/ Second |]                1 (Just "St")
declareDerivedUnit "Kayser"
  [t| Centimeter :^ MOne |]                         1 Nothing

declareDerivedUnit "Maxwell" [t| Nano :@ Weber |]  10  (Just "Mx")
declareDerivedUnit "Gauss"   [t| Milli :@ Tesla |] 0.1 (Just "G")
