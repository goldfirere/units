{-# LANGUAGE TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.SI.MonoTypes
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines type synonyms for SI units, using a Double as the
-- internal representation.
-----------------------------------------------------------------------------

module Data.Metrology.SI.MonoTypes where

import Data.Metrology
import qualified Data.Metrology.SI.Dims as D

type Length              = MkQu_D D.Length
type Mass                = MkQu_D D.Mass
type Time                = MkQu_D D.Time
type Current             = MkQu_D D.Current
type Temperature         = MkQu_D D.Temperature
type AmountOfSubstance   = MkQu_D D.AmountOfSubstance
type LuminousIntensity   = MkQu_D D.LuminousIntensity

type Area                = MkQu_D D.Area
type Volume              = MkQu_D D.Volume
type Velocity            = MkQu_D D.Velocity
type Acceleration        = MkQu_D D.Acceleration
type Wavenumber          = MkQu_D D.Wavenumber
type Density             = MkQu_D D.Density
type SurfaceDensity      = MkQu_D D.SurfaceDensity
type SpecificVolume      = MkQu_D D.SpecificVolume
type CurrentDensity      = MkQu_D D.CurrentDensity
type MagneticStrength    = MkQu_D D.MagneticStrength
type Concentration       = MkQu_D D.Concentration
type Luminance           = MkQu_D D.Luminance
type Frequency           = MkQu_D D.Frequency
type Force               = MkQu_D D.Force
type Pressure            = MkQu_D D.Pressure
type Energy              = MkQu_D D.Energy
type Power               = MkQu_D D.Power
type Charge              = MkQu_D D.Charge
type ElectricPotential   = MkQu_D D.ElectricPotential
type Capacitance         = MkQu_D D.Capacitance
type Resistance          = MkQu_D D.Resistance
type Conductance         = MkQu_D D.Conductance
type MagneticFlux        = MkQu_D D.MagneticFlux
type MagneticFluxDensity = MkQu_D D.MagneticFluxDensity
type Inductance          = MkQu_D D.Inductance
type LuminousFlux        = MkQu_D D.LuminousFlux
type Illuminance         = MkQu_D D.Illuminance
type Kerma               = MkQu_D D.Kerma
type CatalyticActivity   = MkQu_D D.CatalyticActivity
type Momentum            = MkQu_D D.Momentum
