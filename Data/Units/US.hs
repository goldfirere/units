-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.US
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines the American customary system of units. Because
-- there are some names that are conflicted, even within this system,
-- there are several modules underneath here, defining sub-parts of
-- the US system. This module gathers together a subjective set of
-- units users will commonly wish to use. It also exports type instances
-- 'DefaultUnitOfDim' that use the /SI/ internal representations. This
-- choice is made for inter-compatibility with SI computations. If you
-- want the foot-pound-second system, use the 'FPS'.
--
-- Included are all units mentioned here:
-- <http://en.wikipedia.org/wiki/United_States_customary_units>
-- Where possible, conversion rates have been independently verified
-- at a US government website. However, Wikipedia's base is /much/
-- better organized than any government resource immediately available.
-- The US government references used are as follows:
-- <http://nist.gov/pml/wmd/metric/upload/SP1038.pdf>
-- <http://nist.gov/pml/wmd/pubs/upload/appc-14-hb44-final.pdf>
-----------------------------------------------------------------------------

module Data.Units.US (
  -- * Lengths
  Angstrom(..), Mil(..), Point(..), Pica(..),
  Inch(..), Foot(..), Yard(..), Mile(..), NauticalMile(..),

  -- * Velocity
  Knot(..),
  
  -- * Area
  Survey.Acre(..),

  -- * Volume
  -- | These are all /liquid/ measures. Solid measures are /different/.
  Liq.Teaspoon(..), Liq.Tablespoon(..), Liq.FluidOunce(..),
  Liq.Cup(..), Liq.Pint(..), Liq.Quart(..), Liq.Gallon(..),

  -- * Mass
  -- | These are all in the avoirdupois system
  Avdp.Ounce(..), Avdp.Pound(..), Avdp.Ton(..),

  -- * Pressure
  Atmosphere(..), Bar(..),

  -- * Energy
  FoodCalorie(..), Therm(..), Btu(..),

  -- * Power
  Horsepower(..)
  ) where

import Data.Units.US.Misc
import qualified Data.Units.US.Survey      as Survey
import qualified Data.Units.US.Liquid      as Liq
import qualified Data.Units.US.Avoirdupois as Avdp
