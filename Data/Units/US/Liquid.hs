{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.US.Liquid
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines liquid volume measures as used in the USA.
-- Note that liquid volumes in the USA differ both from solid volumes
-- in the USA and from liquid volumes in the UK.
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

module Data.Units.US.Liquid where

import Data.Metrology
import Data.Metrology.TH
import Data.Units.US.Misc

import Language.Haskell.TH

declareDerivedUnit "Gallon"     [t| Inch :^ Three |] 231     (Just "gal")

declareDerivedUnit "FluidOunce" [t| Gallon        |] (1/128) (Just "floz")
declareDerivedUnit "Gill"       [t| FluidOunce    |] 4       (Just "gi")
declareDerivedUnit "Cup"        [t| FluidOunce    |] 8       (Just "cp")
declareDerivedUnit "Pint"       [t| FluidOunce    |] 16      (Just "pt")
declareDerivedUnit "Quart"      [t| Gallon        |] (1/4)   (Just "qt")

declareDerivedUnit "Teaspoon"   [t| FluidOunce    |] (1/6)   (Just "tsp")
declareDerivedUnit "Tablespoon" [t| Teaspoon      |] 3       (Just "Tbsp")
declareDerivedUnit "Shot"       [t| Tablespoon    |] 3       (Just "jig")
declareDerivedUnit "Minim"      [t| Teaspoon      |] (1/80)  (Just "min")
declareDerivedUnit "Dram"       [t| Minim         |] 60      (Just "fldr")

declareDerivedUnit "Hogshead"   [t| Gallon        |] 63      (Just "hogshead")
declareDerivedUnit "Barrel"     [t| Hogshead      |] (1/2)   (Just "bbl")
declareDerivedUnit "OilBarrel"  [t| Gallon        |] 42      (Just "bbl")

-- | As shown on Wikipedia: <http://en.wikipedia.org/wiki/United_States_customary_units>
commonLiquidMeasures :: [Name]
commonLiquidMeasures = [ ''Teaspoon, ''Tablespoon, ''FluidOunce, ''Cup, ''Pint
                       , ''Quart, ''Gallon ]

-- | Includes the rest of the measures in this file.
otherLiquidMeasures :: [Name]
otherLiquidMeasures = [ ''Minim, ''Dram, ''Shot, ''Gill, ''Barrel
                      , ''OilBarrel, ''Hogshead ]
