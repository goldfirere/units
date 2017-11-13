-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.Imperial.Volume
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Due to the <https://en.wikipedia.org/wiki/International_yard_and_pound
-- International yard and pound agreement of 1959>, it so happens that
-- Imperial and US customary units of weight have the same base (the
-- international pound at 0.45359237 kilograms). However, if subdivisions are
-- generally the same, Imperial units feature different multiples of the
-- pound, due to the presence of the stone.
--
-- Where possible, reference have been made to UK legislation. However,
-- Wikipedia's base is /much/ better organized than any government
-- resource immediately available.
-----------------------------------------------------------------------------

module Data.Units.Imperial.Volume where

import Data.Metrology.TH

import Data.Units.SI ( Litre )

import Language.Haskell.TH

declareDerivedUnit "FluidOunce" [t| Gill   |] 0.2     (Just "fl oz")
declareDerivedUnit "Gill"       [t| Pint   |] 0.25    (Just "gi")
declareDerivedUnit "Pint"       [t| Quart  |] 0.5     (Just "pt")
declareDerivedUnit "Quart"      [t| Gallon |] 0.25    (Just "qt")
declareDerivedUnit "Gallon"     [t| Litre  |] 4.54609 (Just "gal")

declareDerivedUnit "Cran" [t| Gallon |] 37.5 (Just "cran")
declareDerivedUnit "Bushel" [t| Gallon |] 8 (Just "bsh")

-- | Includes 'FluidOunce', 'Pint', 'Gallon'
commonVolumeMeasures :: [Name]
commonVolumeMeasures = [ ''FluidOunce, ''Pint, ''Gallon ]

-- | Includes 'Gill', 'Quart', 'Cran' and 'Bushel'
otherVolumeMeasures :: [Name]
otherVolumeMeasures = [ ''Gill, ''Quart, ''Bushel, ''Cran ]
