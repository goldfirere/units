-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.Imperial.Area
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The traditional area units are defined in reference to the survey
-- ones.
--
-- Where possible, reference have been made to UK legislation. However,
-- Wikipedia's page is /much/ better organized than any government
-- resource immediately available.
-----------------------------------------------------------------------------

module Data.Units.Imperial.Area where

import Data.Metrology.TH

import Data.Units.Imperial.Length
import Data.Units.Imperial.Survey

import Language.Haskell.TH

declareDerivedUnit "Perch" [t| Rod :* Rod       |] 1 (Just "perch")
declareDerivedUnit "Rood"  [t| Furlong :* Rod   |] 1 (Just "rood")
declareDerivedUnit "Acre"  [t| Furlong :* Chain |] 1 (Just "acre")

-- | Areas units: All units above
areas :: [Name]
areas = [ ''Perch, ''Rood, ''Acre ]
