{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Imperial.US.Units
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines American imperial units.
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

module Data.Metrology.Imperial.US.Units (
  module Data.Metrology.Imperial.US.Units,

  -- * Re-exports
  -- | The following units are defined in more specialized modules,
  --   but are re-exported here because of common use.
  Survey.Acre(..)
  ) where

import Data.Metrology
import Data.Metrology.TH

import Data.Metrology.SI.Units
import Data.Metrology.SI.Prefixes
import qualified Data.Metrology.Imperial.US.Units.Survey as Survey

import Language.Haskell.TH

----------------------------------------------------------
-- Lengths
----------------------------------------------------------

declareDerivedUnit "Foot"       [t| Meter         |] 0.3048    (Just "ft")
declareDerivedUnit "Inch"       [t| Foot          |] (1/12)    (Just "in")
declareDerivedUnit "Yard"       [t| Foot          |] 3         (Just "yd")
declareDerivedUnit "Mile"       [t| Foot          |] 5280      (Just "mi")
declareDerivedUnit "Angstrom"   [t| Nano :@ Meter |] 0.1       (Just "Å")
declareDerivedUnit "Hand"       [t| Inch          |] 4         (Just "hand")
declareDerivedUnit "Mil"        [t| Inch          |] 0.001     (Just "mil")
lengths :: [Name]
lengths = [ ''Foot, ''Inch, ''Yard, ''Mile, ''Angstrom, ''Mil ]

declareDerivedUnit "Point"      [t| Inch   |] 0.013837    (Just "p")
declareDerivedUnit "Pica"       [t| Point  |] 12          (Just "P")
printerLengths :: [Name]
printerLengths = [''Pica, ''Point]

declareDerivedUnit "Fathom"       [t| Yard       |]    2     (Just "ftm")
declareDerivedUnit "Cable"        [t| Fathom     |]    120   (Just "cb")
declareDerivedUnit "NauticalMile" [t| Kilo :@ Meter |] 1.852 (Just "NM")
nauticalLengths :: [Name]
nauticalLengths = [ ''Fathom, ''Cable, ''NauticalMile ]

declareDerivedUnit "Knot"       [t| NauticalMile :/ Hour |] 1 (Just "kn")

declareDerivedUnit "Atmosphere" [t| Pascal |] 101325    (Just "atm")
declareDerivedUnit "Calorie"    [t| Joule  |] 4.184     (Just "cal")
declareDerivedUnit "Therm"      [t| Joule  |] 105480400 (Just "thm")
declareDerivedUnit "Horsepower" [t| Watt   |] 746       (Just "hp")
