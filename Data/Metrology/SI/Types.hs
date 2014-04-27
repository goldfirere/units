{-# LANGUAGE TypeOperators, DataKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.SI.Types
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines type synonyms for SI units, using a Double as the
-- internal representation.
-----------------------------------------------------------------------------

module Data.Metrology.SI.Types where

import Data.Metrology
import Data.Metrology.SI.Units
import Data.Metrology.SI.Prefixes ( Kilo )
import qualified Data.Metrology.SI.Dims as D

-- defined here to avoid a module dependency
type SI = MkLCSU '[ (D.Length, Meter)
                  , (D.Mass, Kilo :@ Gram)
                  , (D.Time, Second)
                  , (D.Current, Ampere)
                  , (D.Temperature, Kelvin)
                  , (D.AmountOfSubstance, Mole)
                  , (D.LuminousIntensity, Lumen)
                  ]

type Length              = MkQu_DLN D.Length              SI Double
type Mass                = MkQu_DLN D.Mass                SI Double
type Time                = MkQu_DLN D.Time                SI Double
type Current             = MkQu_DLN D.Current             SI Double
type Temperature         = MkQu_DLN D.Temperature         SI Double
type AmountOfSubstance   = MkQu_DLN D.AmountOfSubstance   SI Double
type LuminousIntensity   = MkQu_DLN D.LuminousIntensity   SI Double

type Area                = MkQu_DLN D.Area                SI Double
type Volume              = MkQu_DLN D.Volume              SI Double
type Velocity            = MkQu_DLN D.Velocity            SI Double
type Acceleration        = MkQu_DLN D.Acceleration        SI Double
type Wavenumber          = MkQu_DLN D.Wavenumber          SI Double
type Density             = MkQu_DLN D.Density             SI Double
type SurfaceDensity      = MkQu_DLN D.SurfaceDensity      SI Double
type SpecificVolume      = MkQu_DLN D.SpecificVolume      SI Double
type CurrentDensity      = MkQu_DLN D.CurrentDensity      SI Double
type MagneticStrength    = MkQu_DLN D.MagneticStrength    SI Double
type Concentration       = MkQu_DLN D.Concentration       SI Double
type Luminance           = MkQu_DLN D.Luminance           SI Double
type Frequency           = MkQu_DLN D.Frequency           SI Double
type Force               = MkQu_DLN D.Force               SI Double
type Pressure            = MkQu_DLN D.Pressure            SI Double
type Energy              = MkQu_DLN D.Energy              SI Double
type Power               = MkQu_DLN D.Power               SI Double
type Charge              = MkQu_DLN D.Charge              SI Double
type ElectricPotential   = MkQu_DLN D.ElectricPotential   SI Double
type Capacitance         = MkQu_DLN D.Capacitance         SI Double
type Resistance          = MkQu_DLN D.Resistance          SI Double
type Conductance         = MkQu_DLN D.Conductance         SI Double
type MagneticFlux        = MkQu_DLN D.MagneticFlux        SI Double
type MagneticFluxDensity = MkQu_DLN D.MagneticFluxDensity SI Double
type Inductance          = MkQu_DLN D.Inductance          SI Double
type LuminousFlux        = MkQu_DLN D.LuminousFlux        SI Double
type Illuminance         = MkQu_DLN D.Illuminance         SI Double
type Kerma               = MkQu_DLN D.Kerma               SI Double
type CatalyticActivity   = MkQu_DLN D.CatalyticActivity   SI Double
type Momentum            = MkQu_DLN D.Momentum            SI Double
