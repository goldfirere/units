{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.Imperial.Nautical
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- As per current UK legislation, the nautical mile is defined as 1853 m,
-- not 1852 (as it was previously).
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

module Data.Units.Imperial.Nautical where

import Data.Metrology.TH

import Data.Units.SI

import Language.Haskell.TH

declareDerivedUnit "NauticalMile" [t| Metre        |] 1853 (Just "NM")
declareDerivedUnit "Cable"        [t| NauticalMile |] 0.1  (Just "cable")
declareDerivedUnit "Fathom"       [t| Cable        |] 0.01 (Just "ftm")

-- | Nautical lengths: All units above
nauticalLengths :: [Name]
nauticalLengths = [ ''Fathom, ''Cable, ''NauticalMile ]
