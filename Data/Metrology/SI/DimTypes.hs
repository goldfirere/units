{-# LANGUAGE TypeOperators, DataKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Types
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines type synonyms for dimensions based on the seven
-- SI dimensions, for arbitrary choice of system of units and numerical values.
-----------------------------------------------------------------------------

module Data.Metrology.SI.DimTypes where

import Data.Metrology
import qualified Data.Metrology.SI.Dims as D

type Length              = MkGenQu D.Length              
type Mass                = MkGenQu D.Mass                
type Time                = MkGenQu D.Time                
type Current             = MkGenQu D.Current             
type Temperature         = MkGenQu D.Temperature         
type AmountOfSubstance   = MkGenQu D.AmountOfSubstance   
type LuminousIntensity   = MkGenQu D.LuminousIntensity   

type Area                = MkGenQu D.Area                
type Volume              = MkGenQu D.Volume              
type Velocity            = MkGenQu D.Velocity            
type Acceleration        = MkGenQu D.Acceleration        
type Wavenumber          = MkGenQu D.Wavenumber          
type Density             = MkGenQu D.Density             
type SurfaceDensity      = MkGenQu D.SurfaceDensity      
type SpecificVolume      = MkGenQu D.SpecificVolume      
type CurrentDensity      = MkGenQu D.CurrentDensity      
type MagneticStrength    = MkGenQu D.MagneticStrength    
type Concentration       = MkGenQu D.Concentration       
type Luminance           = MkGenQu D.Luminance           
type Frequency           = MkGenQu D.Frequency           
type Force               = MkGenQu D.Force               
type Pressure            = MkGenQu D.Pressure            
type Energy              = MkGenQu D.Energy              
type Power               = MkGenQu D.Power               
type Charge              = MkGenQu D.Charge              
type ElectricPotential   = MkGenQu D.ElectricPotential   
type Capacitance         = MkGenQu D.Capacitance         
type Resistance          = MkGenQu D.Resistance          
type Conductance         = MkGenQu D.Conductance         
type MagneticFlux        = MkGenQu D.MagneticFlux        
type MagneticFluxDensity = MkGenQu D.MagneticFluxDensity 
type Inductance          = MkGenQu D.Inductance          
type LuminousFlux        = MkGenQu D.LuminousFlux        
type Illuminance         = MkGenQu D.Illuminance         
type Kerma               = MkGenQu D.Kerma               
type CatalyticActivity   = MkGenQu D.CatalyticActivity   
type Momentum            = MkGenQu D.Momentum            
