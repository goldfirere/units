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

type Length              = MkQu_DLN D.Length              Imperial Double
type Mass                = MkQu_DLN D.Mass                Imperial Double
type Time                = MkQu_DLN D.Time                Imperial Double
type Current             = MkQu_DLN D.Current             Imperial Double
type Temperature         = MkQu_DLN D.Temperature         Imperial Double
type AmountOfSubstance   = MkQu_DLN D.AmountOfSubstance   Imperial Double
type LuminousIntensity   = MkQu_DLN D.LuminousIntensity   Imperial Double

type Area                = MkQu_DLN D.Area                Imperial Double
type Volume              = MkQu_DLN D.Volume              Imperial Double
type Velocity            = MkQu_DLN D.Velocity            Imperial Double
type Acceleration        = MkQu_DLN D.Acceleration        Imperial Double
type Wavenumber          = MkQu_DLN D.Wavenumber          Imperial Double
type Density             = MkQu_DLN D.Density             Imperial Double
type SurfaceDensity      = MkQu_DLN D.SurfaceDensity      Imperial Double
type SpecificVolume      = MkQu_DLN D.SpecificVolume      Imperial Double
type CurrentDensity      = MkQu_DLN D.CurrentDensity      Imperial Double
type MagneticStrength    = MkQu_DLN D.MagneticStrength    Imperial Double
type Concentration       = MkQu_DLN D.Concentration       Imperial Double
type Luminance           = MkQu_DLN D.Luminance           Imperial Double
type Frequency           = MkQu_DLN D.Frequency           Imperial Double
type Force               = MkQu_DLN D.Force               Imperial Double
type Pressure            = MkQu_DLN D.Pressure            Imperial Double
type Energy              = MkQu_DLN D.Energy              Imperial Double
type Power               = MkQu_DLN D.Power               Imperial Double
type Charge              = MkQu_DLN D.Charge              Imperial Double
type ElectricPotential   = MkQu_DLN D.ElectricPotential   Imperial Double
type Capacitance         = MkQu_DLN D.Capacitance         Imperial Double
type Resistance          = MkQu_DLN D.Resistance          Imperial Double
type Conductance         = MkQu_DLN D.Conductance         Imperial Double
type MagneticFlux        = MkQu_DLN D.MagneticFlux        Imperial Double
type MagneticFluxDensity = MkQu_DLN D.MagneticFluxDensity Imperial Double
type Inductance          = MkQu_DLN D.Inductance          Imperial Double
type Illuminance         = MkQu_DLN D.Illuminance         Imperial Double
type Kerma               = MkQu_DLN D.Kerma               Imperial Double
type CatalyticActivity   = MkQu_DLN D.CatalyticActivity   Imperial Double
type Momentum            = MkQu_DLN D.Momentum            Imperial Double
