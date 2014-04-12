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
    , (D.Quantity, Mole)
    , (D.Luminosity, Candela)
    ]

type Length              = MkGenDim D.Length              Imperial Double
type Mass                = MkGenDim D.Mass                Imperial Double
type Time                = MkGenDim D.Time                Imperial Double
type Current             = MkGenDim D.Current             Imperial Double
type Temperature         = MkGenDim D.Temperature         Imperial Double
type Quantity            = MkGenDim D.Quantity            Imperial Double
type Luminosity          = MkGenDim D.Luminosity          Imperial Double

type Area                = MkGenDim D.Area                Imperial Double
type Volume              = MkGenDim D.Volume              Imperial Double
type Velocity            = MkGenDim D.Velocity            Imperial Double
type Acceleration        = MkGenDim D.Acceleration        Imperial Double
type Wavenumber          = MkGenDim D.Wavenumber          Imperial Double
type Density             = MkGenDim D.Density             Imperial Double
type SurfaceDensity      = MkGenDim D.SurfaceDensity      Imperial Double
type SpecificVolume      = MkGenDim D.SpecificVolume      Imperial Double
type CurrentDensity      = MkGenDim D.CurrentDensity      Imperial Double
type MagneticStrength    = MkGenDim D.MagneticStrength    Imperial Double
type Concentration       = MkGenDim D.Concentration       Imperial Double
type Luminance           = MkGenDim D.Luminance           Imperial Double
type Frequency           = MkGenDim D.Frequency           Imperial Double
type Force               = MkGenDim D.Force               Imperial Double
type Pressure            = MkGenDim D.Pressure            Imperial Double
type Energy              = MkGenDim D.Energy              Imperial Double
type Power               = MkGenDim D.Power               Imperial Double
type Charge              = MkGenDim D.Charge              Imperial Double
type ElectricPotential   = MkGenDim D.ElectricPotential   Imperial Double
type Capacitance         = MkGenDim D.Capacitance         Imperial Double
type Resistance          = MkGenDim D.Resistance          Imperial Double
type Conductance         = MkGenDim D.Conductance         Imperial Double
type MagneticFlux        = MkGenDim D.MagneticFlux        Imperial Double
type MagneticFluxDensity = MkGenDim D.MagneticFluxDensity Imperial Double
type Inductance          = MkGenDim D.Inductance          Imperial Double
type Illuminance         = MkGenDim D.Illuminance         Imperial Double
type Kerma               = MkGenDim D.Kerma               Imperial Double
type CatalyticActivity   = MkGenDim D.CatalyticActivity   Imperial Double
type Momentum            = MkGenDim D.Momentum            Imperial Double
