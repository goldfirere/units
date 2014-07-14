{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Imperial.US.Units.DryVolume
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines dry volume measures as used in the USA.
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

module Data.Metrology.Imperial.US.Units.DryVolume where

import Data.Metrology
import Data.Metrology.TH
import Data.Metrology.Imperial.US.Units

import Language.Haskell.TH

declareDerivedUnit "Gallon" [t| Inch :^ Three |] 268.8025 (Just "gal")
declareDerivedUnit "Quart"  [t| Gallon        |] (1/4)    (Just "qt")
declareDerivedUnit "Pint"   [t| Quart         |] (1/2)    (Just "pt")
declareDerivedUnit "Peck"   [t| Gallon        |] 2        (Just "pk")
declareDerivedUnit "Bushel" [t| Peck          |] 4        (Just "bu")
declareDerivedUnit "Barrel" [t| Inch :^ Three |] 7056     (Just "bbl")
declareDerivedUnit "Cord"   [t| Foot :^ Three |] 128      (Just "cd")

declareDerivedUnit "CranberryBarrel" [t| Inch :^ Three |] 5826 (Just "bbl")

dryVolumeMeasures :: [Name]
dryVolumeMeasures = [ ''Pint, ''Quart, ''Gallon, ''Peck, ''Bushel
                    , ''Barrel, ''Cord ]
