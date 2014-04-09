{-# LANGUAGE TypeOperators, DataKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dimensions.SI.Types
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines type synonyms for SI units, using a Double as the
-- internal representation.
-----------------------------------------------------------------------------

module Data.Dimensions.SI.Types where

import Data.Dimensions
import Data.Dimensions.SI.Units
import Data.Dimensions.SI.Prefixes ( Kilo )
import qualified Data.Dimensions.SI.Dims as D

-- defined here to avoid a module dependency
type SI = MkLCSU '[ (D.Length, Meter)
                  , (D.Mass, Kilo :@ Gram)
                  , (D.Time, Second)
                  , (D.Current, Ampere)
                  , (D.Temperature, Kelvin)
                  , (D.Quantity, Mole)
                  , (D.Luminosity, Lumen)
                  ]

type Length              = MkGenDim D.Length              SI Double
type Mass                = MkGenDim D.Mass                SI Double
type Time                = MkGenDim D.Time                SI Double
type Current             = MkGenDim D.Current             SI Double
type Temperature         = MkGenDim D.Temperature         SI Double
type Quantity            = MkGenDim D.Quantity            SI Double
type Luminosity          = MkGenDim D.Luminosity          SI Double

type Area                = MkGenDim D.Area                SI Double
type Volume              = MkGenDim D.Volume              SI Double
type Velocity            = MkGenDim D.Velocity            SI Double
type Acceleration        = MkGenDim D.Acceleration        SI Double
type Wavenumber          = MkGenDim D.Wavenumber          SI Double
type Density             = MkGenDim D.Density             SI Double
type SurfaceDensity      = MkGenDim D.SurfaceDensity      SI Double
type SpecificVolume      = MkGenDim D.SpecificVolume      SI Double
type CurrentDensity      = MkGenDim D.CurrentDensity      SI Double
type MagneticStrength    = MkGenDim D.MagneticStrength    SI Double
type Concentration       = MkGenDim D.Concentration       SI Double
type Luminance           = MkGenDim D.Luminance           SI Double
type Frequency           = MkGenDim D.Frequency           SI Double
type Force               = MkGenDim D.Force               SI Double
type Pressure            = MkGenDim D.Pressure            SI Double
type Energy              = MkGenDim D.Energy              SI Double
type Power               = MkGenDim D.Power               SI Double
type Charge              = MkGenDim D.Charge              SI Double
type ElectricPotential   = MkGenDim D.ElectricPotential   SI Double
type Capacitance         = MkGenDim D.Capacitance         SI Double
type Resistance          = MkGenDim D.Resistance          SI Double
type Conductance         = MkGenDim D.Conductance         SI Double
type MagneticFlux        = MkGenDim D.MagneticFlux        SI Double
type MagneticFluxDensity = MkGenDim D.MagneticFluxDensity SI Double
type Inductance          = MkGenDim D.Inductance          SI Double
type LuminousFlux        = MkGenDim D.LuminousFlux        SI Double
type Illuminance         = MkGenDim D.Illuminance         SI Double
type Kerma               = MkGenDim D.Kerma               SI Double
type CatalyticActivity   = MkGenDim D.CatalyticActivity   SI Double
type Momentum            = MkGenDim D.Momentum            SI Double
