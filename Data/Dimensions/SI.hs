{-# LANGUAGE TypeOperators, TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dimensions.SI
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines SI dimensions. The names of SI dimensions conform to
-- <http://www.bipm.org/utils/common/documents/jcgm/JCGM_200_2012.pdf>.
-----------------------------------------------------------------------------

module Data.Dimensions.SI where

import Data.Metrology.Poly
import Data.Metrology.TH

declareDimension "Length"
declareDimension "Mass"
declareDimension "Time"
declareDimension "Current"
declareDimension "Temperature"
declareDimension "AmountOfSubstance"
declareDimension "LuminousIntensity"

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
type Concentration       = AmountOfSubstance          :/ Volume
type Luminance           = LuminousIntensity        :/ Area

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
type LuminousFlux        = LuminousIntensity
type Illuminance         = LuminousIntensity :/ Area
type Kerma               = Area              :/ (Time :^ Two)
type CatalyticActivity   = AmountOfSubstance :/ Time

type Momentum            = Mass              :* Velocity
