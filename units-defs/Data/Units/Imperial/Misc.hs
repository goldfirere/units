{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.Imperial.Misc
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Miscellaneous units not easily put in another module.
--
-- Where possible, reference have been made to UK legislation. However,
-- Wikipedia's page is /much/ better organized than any government
-- resource immediately available.
--
-- The UK legislation used as references are as follows:
-- <http://www.legislation.gov.uk/ukpga/1985/72/enacted>
-- <http://www.legislation.gov.uk/uksi/1994/2867/schedule/part/VI/made>
-- <http://www.legislation.gov.uk/uksi/1995/1804/schedule/made>
-----------------------------------------------------------------------------

module Data.Units.Imperial.Misc where

import Data.Metrology
import Data.Metrology.TH

import Data.Units.SI
import Data.Units.Imperial.Nautical

import Language.Haskell.TH

declareDerivedUnit "Knot" [t| NauticalMile :/ Hour |] 1 (Just "kn")

declareDerivedUnit "InchOfWater" [t| Pascal |] 249.08891 (Just "inAq")

declareDerivedUnit "PoundForce" [t| Newton     |] 4.4482216152605  (Just "lbf")
declareDerivedUnit "TonForce"   [t| PoundForce |] 2240             (Just "tf")

declareDerivedUnit "Horsepower" [t| Watt |] 745.69987158227022 (Just "hp")

declareDerivedUnit "FootPoundForce"     [t| Joule |]              1.3558179483314004 (Just "ft⋅lbf")
declareDerivedUnit "BritishThermalUnit" [t| Joule |]              1055.05585257348   (Just "Btu")
declareDerivedUnit "Therm"              [t| BritishThermalUnit |] 100000             (Just "thm")

declareDerivedUnit "FootCandle" [t| Lux |] 10.763910416709 (Just "fc")

declareDerivedUnit "Fahrenheit" [t| Kelvin |] (5/9)  (Just "°F")

-- | Speed units: 'Knot'
speeds :: [Name]
speeds = [ ''Knot ]

-- | Temperature units: 'Fahrenheit'
temperatures :: [Name]
temperatures = [ ''Fahrenheit ]

-- | Illuminance units: 'FootCandle'
illuminances :: [Name]
illuminances = [ ''FootCandle ]

-- | Force units: 'PoundForce' and 'TonForce'
forces :: [Name]
forces = [ ''PoundForce, ''TonForce ]

-- | Energy units: 'FootPoundForce', 'BritishThermalUnit' and 'Therm'
energies :: [Name]
energies = [ ''FootPoundForce, ''BritishThermalUnit, ''Therm ]

-- | Power units: 'Horsepower'
powers :: [Name]
powers = [ ''Horsepower ]
