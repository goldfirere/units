{-# LANGUAGE TypeOperators, DataKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Imperial.Types
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines type synonyms for British Imperial units.
-----------------------------------------------------------------------------
module Data.Metrology.Imperial.Types where

import Data.Metrology
import Data.Metrology.SI.Units
import Data.Metrology.Imperial.Units (Yard, Pound)
import qualified Data.Metrology.SI.Dims as D


type Imperial = 
  MkLCSU 
   '[ (D.Length, Yard)
    , (D.Mass, Pound)
    , (D.Time, Second)
    , (D.Current, Ampere)
    , (D.Temperature, Kelvin)
    , (D.AmountOfSubstance, Mole)
    , (D.LuminousIntensity, Candela)
    ]

type Length              = MkGenQu D.Length              Imperial Double
type Mass                = MkGenQu D.Mass                Imperial Double
type Time                = MkGenQu D.Time                Imperial Double
type Current             = MkGenQu D.Current             Imperial Double
type Temperature         = MkGenQu D.Temperature         Imperial Double
type AmountOfSubstance   = MkGenQu D.AmountOfSubstance   Imperial Double
type LuminousIntensity   = MkGenQu D.LuminousIntensity   Imperial Double

type Area                = MkGenQu D.Area                Imperial Double
type Volume              = MkGenQu D.Volume              Imperial Double
type Velocity            = MkGenQu D.Velocity            Imperial Double
type Acceleration        = MkGenQu D.Acceleration        Imperial Double
type Wavenumber          = MkGenQu D.Wavenumber          Imperial Double
type Density             = MkGenQu D.Density             Imperial Double
type SurfaceDensity      = MkGenQu D.SurfaceDensity      Imperial Double
type SpecificVolume      = MkGenQu D.SpecificVolume      Imperial Double
type CurrentDensity      = MkGenQu D.CurrentDensity      Imperial Double
type MagneticStrength    = MkGenQu D.MagneticStrength    Imperial Double
type Concentration       = MkGenQu D.Concentration       Imperial Double
type Luminance           = MkGenQu D.Luminance           Imperial Double
type Frequency           = MkGenQu D.Frequency           Imperial Double
type Force               = MkGenQu D.Force               Imperial Double
type Pressure            = MkGenQu D.Pressure            Imperial Double
type Energy              = MkGenQu D.Energy              Imperial Double
type Power               = MkGenQu D.Power               Imperial Double
type Charge              = MkGenQu D.Charge              Imperial Double
type ElectricPotential   = MkGenQu D.ElectricPotential   Imperial Double
type Capacitance         = MkGenQu D.Capacitance         Imperial Double
type Resistance          = MkGenQu D.Resistance          Imperial Double
type Conductance         = MkGenQu D.Conductance         Imperial Double
type MagneticFlux        = MkGenQu D.MagneticFlux        Imperial Double
type MagneticFluxDensity = MkGenQu D.MagneticFluxDensity Imperial Double
type Inductance          = MkGenQu D.Inductance          Imperial Double
type Illuminance         = MkGenQu D.Illuminance         Imperial Double
type Kerma               = MkGenQu D.Kerma               Imperial Double
type CatalyticActivity   = MkGenQu D.CatalyticActivity   Imperial Double
type Momentum            = MkGenQu D.Momentum            Imperial Double
