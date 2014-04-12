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

type Length              = MkGenQu D.Length              SI Double
type Mass                = MkGenQu D.Mass                SI Double
type Time                = MkGenQu D.Time                SI Double
type Current             = MkGenQu D.Current             SI Double
type Temperature         = MkGenQu D.Temperature         SI Double
type AmountOfSubstance   = MkGenQu D.AmountOfSubstance   SI Double
type LuminousIntensity   = MkGenQu D.LuminousIntensity   SI Double

type Area                = MkGenQu D.Area                SI Double
type Volume              = MkGenQu D.Volume              SI Double
type Velocity            = MkGenQu D.Velocity            SI Double
type Acceleration        = MkGenQu D.Acceleration        SI Double
type Wavenumber          = MkGenQu D.Wavenumber          SI Double
type Density             = MkGenQu D.Density             SI Double
type SurfaceDensity      = MkGenQu D.SurfaceDensity      SI Double
type SpecificVolume      = MkGenQu D.SpecificVolume      SI Double
type CurrentDensity      = MkGenQu D.CurrentDensity      SI Double
type MagneticStrength    = MkGenQu D.MagneticStrength    SI Double
type Concentration       = MkGenQu D.Concentration       SI Double
type Luminance           = MkGenQu D.Luminance           SI Double
type Frequency           = MkGenQu D.Frequency           SI Double
type Force               = MkGenQu D.Force               SI Double
type Pressure            = MkGenQu D.Pressure            SI Double
type Energy              = MkGenQu D.Energy              SI Double
type Power               = MkGenQu D.Power               SI Double
type Charge              = MkGenQu D.Charge              SI Double
type ElectricPotential   = MkGenQu D.ElectricPotential   SI Double
type Capacitance         = MkGenQu D.Capacitance         SI Double
type Resistance          = MkGenQu D.Resistance          SI Double
type Conductance         = MkGenQu D.Conductance         SI Double
type MagneticFlux        = MkGenQu D.MagneticFlux        SI Double
type MagneticFluxDensity = MkGenQu D.MagneticFluxDensity SI Double
type Inductance          = MkGenQu D.Inductance          SI Double
type LuminousFlux        = MkGenQu D.LuminousFlux        SI Double
type Illuminance         = MkGenQu D.Illuminance         SI Double
type Kerma               = MkGenQu D.Kerma               SI Double
type CatalyticActivity   = MkGenQu D.CatalyticActivity   SI Double
type Momentum            = MkGenQu D.Momentum            SI Double
