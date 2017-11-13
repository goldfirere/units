-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.Imperial
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines the Imperial system of units, based on the
-- Exchequer Standards of 1825. Two big differences between the
-- Imperial system and the US customary one are that because the
-- USA declared their independance from the UK in 1776, the US
-- customary system is based on the previous Winchester Standards, and
-- that the Imperial systems uses the same units for dry and fluid
-- measures. This module gathers together a subjective set of units of
-- general use appearing <https://en.wikipedia.org/wiki/Imperial_units
-- here>, submodules will gather measures of more limited use. It also
-- exports type instances 'DefaultUnitOfDim' that use the /SI/ internal
-- representations. This choice is made for inter-compatibility with SI
-- computations. If you want the foot-pound-second system, use the 'FPS'.
--
-- Where possible, reference have been made to UK legislation. However,
-- Wikipedia's base is /much/ better organized than any government
-- resource immediately available.
-- The UK legislation used as reference is as follows:
-- <http://www.legislation.gov.uk/uksi/1995/1804/made>
-----------------------------------------------------------------------------

module Data.Units.Imperial (
  -- * Lengths
  Thou(..), Inch(..), Foot(..), Yard(..), Mile(..), NauticalMile(..),

  -- * Velocity
  Knot(..),
  
  -- * Area
  Acre(..),

  -- * Volume
  FluidOunce(..), Pint(..), Quart(..), Gallon(..),

  -- * Mass
  -- | These are all in the avoirdupois system
  Ounce(..), Pound(..), Stone(..), Ton(..),

  -- * Pressure
  InchOfWater(..),

  -- * Energy
  FootPoundForce(..), Therm(..), BritishThermalUnit(..),

  -- * Power
  Horsepower(..),

  -- * Temperature
  Fahrenheit(..)
  ) where

import Data.Units.Imperial.Misc
import Data.Units.Imperial.Nautical
import Data.Units.Imperial.Weight
import Data.Units.Imperial.Volume
import Data.Units.Imperial.Area
