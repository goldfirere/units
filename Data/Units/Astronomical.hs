-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.Astronomical
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The system of astronomical units, as defined by the International
-- Astronomical Union in 1976 and since then updated a few times.
--
-- The IAU documents used as reference are as follow:
-- <http://www.iau.org/static/resolutions/IAU1976_French.pdf>
-- <http://www.iau.org/static/resolutions/IAU1994_French.pdf>
-- <http://syrte.obspm.fr/IAU_resolutions/Res_IAU2012_B2.pdf>
-- <https://www.iau.org/publications/proceedings_rules/units/>
-----------------------------------------------------------------------------

module Data.Units.Astronomical (
  -- * Lengths
  AstronomicalUnit(..), LightYear(..), Parsec(..),

  -- * Time
  Day(..), JulianYear(..), JulianCentury(..),

  -- * Mass
  SolarMass(..), JovianMass(..), EarthMass(..)
  ) where

import Data.Constants.Math
import Data.Metrology.TH
import Data.Units.SI
import Data.Units.SI.Prefixes

declareDerivedUnit "Day"           [t| Second |]        86400 (Just "D")
declareDerivedUnit "JulianYear"    [t| JulianCentury |] 0.01  (Just "a")
declareDerivedUnit "JulianCentury" [t| Day |]           36525 (Just "julian century")

declareDerivedUnit "AstronomicalUnit" [t| Metre            |] 149597870700     (Just "au")
declareDerivedUnit "LightYear"        [t| Metre            |] 9460730472580800 (Just "ly")
declareDerivedUnit "Parsec"           [t| AstronomicalUnit |] 648000 / piR     (Just "pc")

declareDerivedUnit "EarthMass"  [t| SolarMass |] 1 / 332946.0487     (Just "E")
declareDerivedUnit "JovianMass" [t| SolarMass |] 1 / 1047.348644     (Just "MJ")
declareDerivedUnit "SolarMass"  [t| Kilo :@ Gram |] 1.9891 * 10 ^ 30 (Just "S")
