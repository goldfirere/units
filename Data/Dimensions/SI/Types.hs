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
-- This module defines type synonyms for SI units.
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

type Length              = MkDim D.Length              SI
type Mass                = MkDim D.Mass                SI
type Time                = MkDim D.Time                SI
type Current             = MkDim D.Current             SI
type Temperature         = MkDim D.Temperature         SI
type Quantity            = MkDim D.Quantity            SI
type Luminosity          = MkDim D.Luminosity          SI

type Area                = MkDim D.Area                SI
type Volume              = MkDim D.Volume              SI
type Velocity            = MkDim D.Velocity            SI
type Acceleration        = MkDim D.Acceleration        SI
type Wavenumber          = MkDim D.Wavenumber          SI
type Density             = MkDim D.Density             SI
type SurfaceDensity      = MkDim D.SurfaceDensity      SI
type SpecificVolume      = MkDim D.SpecificVolume      SI
type CurrentDensity      = MkDim D.CurrentDensity      SI
type MagneticStrength    = MkDim D.MagneticStrength    SI
type Concentration       = MkDim D.Concentration       SI
type Luminance           = MkDim D.Luminance           SI
type Frequency           = MkDim D.Frequency           SI
type Force               = MkDim D.Force               SI
type Pressure            = MkDim D.Pressure            SI
type Energy              = MkDim D.Energy              SI
type Power               = MkDim D.Power               SI
type Charge              = MkDim D.Charge              SI
type ElectricPotential   = MkDim D.ElectricPotential   SI
type Capacitance         = MkDim D.Capacitance         SI
type Resistance          = MkDim D.Resistance          SI
type Conductance         = MkDim D.Conductance         SI
type MagneticFlux        = MkDim D.MagneticFlux        SI
type MagneticFluxDensity = MkDim D.MagneticFluxDensity SI
type Inductance          = MkDim D.Inductance          SI
type LuminousFlux        = MkDim D.LuminousFlux        SI
type Illuminance         = MkDim D.Illuminance         SI
type Kerma               = MkDim D.Kerma               SI
type CatalyticActivity   = MkDim D.CatalyticActivity   SI
type Momentum            = MkDim D.Momentum            SI