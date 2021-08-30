{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.Imperial.Volume
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- There's only one set of volume units in the Imperial system, hence the
-- one ´Volume´ module to the two (´Liquid´ and ´DryVolume´) in the US system.
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

module Data.Units.Imperial.Volume where

import Data.Metrology.TH
import Data.Units.SI

import Language.Haskell.TH

declareDerivedUnit "Gallon"     [t| Litre  |] 4.54609 (Just "gal")
declareDerivedUnit "Quart"      [t| Gallon |] 0.25    (Just "qt")
declareDerivedUnit "Pint"       [t| Quart  |] 0.5     (Just "pt")
declareDerivedUnit "Gill"       [t| Pint   |] 0.25    (Just "gi")
declareDerivedUnit "FluidOunce" [t| Gill   |] 0.2     (Just "fl oz")

declareDerivedUnit "Cran"   [t| Gallon |] 37.5 (Just "cran")
declareDerivedUnit "Bushel" [t| Gallon |] 8    (Just "bsh")

-- | Includes 'FluidOunce', 'Pint', 'Gallon'
commonVolumeMeasures :: [Name]
commonVolumeMeasures = [ ''FluidOunce, ''Pint, ''Gallon ]

-- | Includes 'Gill', 'Quart', 'Cran' and 'Bushel'
otherVolumeMeasures :: [Name]
otherVolumeMeasures = [ ''Gill, ''Quart, ''Bushel, ''Cran ]
