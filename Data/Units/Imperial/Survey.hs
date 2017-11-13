-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.Imperial.Survey
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Survey units are Gunter's units, so called because thay have been
-- standardised by Edmund Gunter in 1620 at 25 links to the rod, 4 rods
-- to the chain.
--
-- Where possible, reference have been made to UK legislation. However,
-- Wikipedia's page is /much/ better organized than any government
-- resource immediately available.
-----------------------------------------------------------------------------

module Data.Units.Imperial.Survey where

import Data.Metrology.TH

import Data.Units.Imperial.Length

import Language.Haskell.TH

declareDerivedUnit "Link" [t| Rod   |] 0.04 (Just "li")
declareDerivedUnit "Rod"  [t| Chain |] 0.25 (Just "rd")

-- | Survey lengths: All units above
surveyLengths :: [Name]
surveyLengths = [ ''Link, ''Rod ]
