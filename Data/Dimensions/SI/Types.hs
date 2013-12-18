{-# LANGUAGE TypeOperators #-}

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

type Length              = MkDim Meter
type Mass                = MkDim Gram
type Time                = MkDim Second
type Current             = MkDim Ampere
type Temperature         = MkDim Kelvin
type Quantity            = MkDim Mole
type Luminosity          = MkDim Candela

type Area                = Length     %^ Two
type Volume              = Length     %^ Three
type Velocity            = Length     %/ Time
type Acceleration        = Length     %/ (Time %^ Two)
type Wavenumber          = Length     %^ MOne
type Density             = Mass       %/ Volume
type SurfaceDensity      = Mass       %/ Area
type SpecificVolume      = Volume     %/ Mass
type CurrentDensity      = Current    %/ Area
type MagneticStrength    = Current    %/ Length
type Concentration       = Quantity   %/ Volume
type Luminance           = Luminosity %/ Area

type Frequency           = MkDim Hertz
type Force               = MkDim Newton
type Pressure            = MkDim Pascal
type Energy              = MkDim Joule
type Power               = MkDim Watt
type Charge              = MkDim Coulomb
type ElectricPotential   = MkDim Volt
type Capacitance         = MkDim Farad
type Resistance          = MkDim Ohm
type Conductance         = MkDim Siemens
type MagneticFlux        = MkDim Weber
type MagneticFluxDensity = MkDim Tesla
type Inductance          = MkDim Henry
type LuminousFlux        = MkDim Lumen
type Illuminance         = MkDim Lux
type Kerma               = MkDim Gray
type CatalyticActivity   = MkDim Katal

type Momentum            = Mass %* Velocity