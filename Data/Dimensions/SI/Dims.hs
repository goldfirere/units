{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dimensions.SI.Dims
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines SI dimensions.
-----------------------------------------------------------------------------

module Data.Dimensions.SI.Dims where

import Data.Dimensions

data Length = Length
instance Dimension Length

data Mass = Mass
instance Dimension Mass

data Time = Time
instance Dimension Time

data Current = Current
instance Dimension Current

data Temperature = Temperature
instance Dimension Temperature

data Quantity = Quantity
instance Dimension Quantity

data Luminosity = Luminosity
instance Dimension Luminosity

type Area                = Length            :^ Two
type Volume              = Length            :^ Three
type Velocity            = Length            :/ Time
type Acceleration        = Velocity          :/ Time
type Wavenumber          = Length            :^ MOne
type Density             = Mass              :/ Volume
type SurfaceDensity      = Mass              :/ Area
type SpecificVolume      = Volume            :/ Mass
type CurrentDensity      = Current           :/ Area
type MagneticStrength    = Current           :/ Length
type Concentration       = Quantity          :/ Volume
type Luminance           = Luminosity        :/ Area

type Frequency           = Time              :^ MOne
type Force               = Mass              :* Acceleration
type Pressure            = Force             :/ Area
type Energy              = Force             :* Length
type Power               = Energy            :/ Time
type Charge              = Current           :* Time
type ElectricPotential   = Power             :/ Current
type Capacitance         = Charge            :/ ElectricPotential
type Resistance          = ElectricPotential :/ Current
type Conductance         = Current           :/ ElectricPotential
type MagneticFlux        = ElectricPotential :* Time
type MagneticFluxDensity = MagneticFlux      :/ Area
type Inductance          = MagneticFlux      :/ Current
type LuminousFlux        = Luminosity
type Illuminance         = Luminosity        :/ Area
type Kerma               = Area              :/ (Time :^ Two)
type CatalyticActivity   = Quantity          :/ Time

type Momentum            = Mass              :* Velocity