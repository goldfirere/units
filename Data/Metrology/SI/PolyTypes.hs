{-# LANGUAGE TypeOperators, DataKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.SI.PolyTypes
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines type synonyms for dimensions based on the seven
-- SI dimensions, for arbitrary choice of system of units and numerical values.
-----------------------------------------------------------------------------

module Data.Metrology.SI.PolyTypes where

import Data.Metrology
import qualified Data.Dimensions.SI as D

type Length              = MkQu_DLN D.Length
type Mass                = MkQu_DLN D.Mass
type Time                = MkQu_DLN D.Time
type Current             = MkQu_DLN D.Current
type Temperature         = MkQu_DLN D.Temperature
type AmountOfSubstance   = MkQu_DLN D.AmountOfSubstance
type LuminousIntensity   = MkQu_DLN D.LuminousIntensity

type Area                = MkQu_DLN D.Area
type Volume              = MkQu_DLN D.Volume
type Velocity            = MkQu_DLN D.Velocity
type Acceleration        = MkQu_DLN D.Acceleration
type Wavenumber          = MkQu_DLN D.Wavenumber
type Density             = MkQu_DLN D.Density
type SurfaceDensity      = MkQu_DLN D.SurfaceDensity
type SpecificVolume      = MkQu_DLN D.SpecificVolume
type CurrentDensity      = MkQu_DLN D.CurrentDensity
type MagneticStrength    = MkQu_DLN D.MagneticStrength
type Concentration       = MkQu_DLN D.Concentration
type Luminance           = MkQu_DLN D.Luminance
type Frequency           = MkQu_DLN D.Frequency
type Force               = MkQu_DLN D.Force
type Pressure            = MkQu_DLN D.Pressure
type Energy              = MkQu_DLN D.Energy
type Power               = MkQu_DLN D.Power
type Charge              = MkQu_DLN D.Charge
type ElectricPotential   = MkQu_DLN D.ElectricPotential
type Capacitance         = MkQu_DLN D.Capacitance
type Resistance          = MkQu_DLN D.Resistance
type Conductance         = MkQu_DLN D.Conductance
type MagneticFlux        = MkQu_DLN D.MagneticFlux
type MagneticFluxDensity = MkQu_DLN D.MagneticFluxDensity
type Inductance          = MkQu_DLN D.Inductance
type LuminousFlux        = MkQu_DLN D.LuminousFlux
type Illuminance         = MkQu_DLN D.Illuminance
type Kerma               = MkQu_DLN D.Kerma
type CatalyticActivity   = MkQu_DLN D.CatalyticActivity
type Momentum            = MkQu_DLN D.Momentum
