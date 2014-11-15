{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.US.Misc
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines American customary units that don't fit into
-- other categories.
--
-- Included are all units mentioned here:
-- http://en.wikipedia.org/wiki/United_States_customary_units
-- Where possible, conversion rates have been independently verified
-- at a US government website. However, Wikipedia's base is /much/
-- better organized than any government resource immediately available.
-- The US government references used are as follows:
-- http://nist.gov/pml/wmd/metric/upload/SP1038.pdf
-- http://nist.gov/pml/wmd/pubs/upload/appc-14-hb44-final.pdf
-----------------------------------------------------------------------------

module Data.Units.US.Misc where

import Data.Metrology
import Data.Metrology.TH

import Data.Metrology.SI.Units
import Data.Metrology.SI.Prefixes
import Data.Constants.Math

import Language.Haskell.TH

declareDerivedUnit "Foot"       [t| Meter         |] 0.3048    (Just "ft")
declareDerivedUnit "Inch"       [t| Foot          |] (1/12)    (Just "in")
declareDerivedUnit "Yard"       [t| Foot          |] 3         (Just "yd")
declareDerivedUnit "Mile"       [t| Foot          |] 5280      (Just "mi")
declareDerivedUnit "Angstrom"   [t| Nano :@ Meter |] 0.1       (Just "Å")
declareDerivedUnit "Hand"       [t| Inch          |] 4         (Just "hand")
declareDerivedUnit "Mil"        [t| Inch          |] 0.001     (Just "mil")

declareDerivedUnit "Point"      [t| Inch   |] 0.013837    (Just "p")
declareDerivedUnit "Pica"       [t| Point  |] 12          (Just "P")

declareDerivedUnit "Fathom"       [t| Yard       |]    2     (Just "ftm")
declareDerivedUnit "Cable"        [t| Fathom     |]    120   (Just "cb")
declareDerivedUnit "NauticalMile" [t| Kilo :@ Meter |] 1.852 (Just "NM")

declareDerivedUnit "Knot"       [t| NauticalMile :/ Hour |] 1 (Just "kn")

declareDerivedUnit "Atmosphere" [t| Kilo :@ Pascal |]  101.325 (Just "atm")
declareDerivedUnit "Bar"        [t| Kilo :@ Pascal |]  100     (Just "bar")
declareDerivedUnit "MillimeterOfMercury"
                                [t| Pascal |] 133.322387415 (Just "mmHg")
declareDerivedUnit "Torr"       [t| Atmosphere |]      (1/760) (Just "Torr")


declareDerivedUnit "Calorie"     [t| Joule           |] 4.184          (Just "cal")
declareDerivedUnit "FoodCalorie" [t| Kilo :@ Calorie |] 1              (Just "Cal")
declareDerivedUnit "Therm"       [t| Mega :@ Joule   |] 105.4804       (Just "thm")
declareDerivedUnit "Btu"         [t| Joule           |] 1055.05585262  (Just "btu")

declareDerivedUnit "Horsepower" [t| Watt   |] 746       (Just "hp")

declareDerivedUnit "Rankine"    [t| Kelvin |] (5/9)  (Just "°R")

declareDerivedUnit "PoundForce" [t| Newton |] 4.4482216152605 (Just "lbf")

declareDerivedUnit "Slug"       [t| PoundForce :* (Second :^ Two) :/ Foot |]
                                1 (Just "slug")

declareDerivedUnit "Oersted"    [t| Ampere :/ Meter |]
                                (1000 / (4 * piR)) (Just "Oe")

declareDerivedUnit "Maxwell"    

-- | Standard lengths: 'Foot', 'Inch', 'Yard', and 'Mile'
lengths :: [Name]
lengths = [ ''Foot, ''Inch, ''Yard, ''Mile ]


