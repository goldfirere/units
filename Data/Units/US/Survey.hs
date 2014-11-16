{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.US.Survey
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines survey measures as used in the USA.
-- Note that a survey foot is ever so slightly different from a standard
-- foot.
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

module Data.Units.US.Survey where

import Data.Metrology
import Data.Metrology.TH
import Data.Units.SI

import Language.Haskell.TH

declareDerivedUnit "Foot"    [t| Meter    |] (1200/3937) (Just "ft")
declareDerivedUnit "Link"    [t| Foot     |] 0.66        (Just "li")
declareDerivedUnit "Rod"     [t| Link     |] 25          (Just "rd")
declareDerivedUnit "Chain"   [t| Rod      |] 4           (Just "ch")
declareDerivedUnit "Furlong" [t| Chain    |] 10          (Just "fur")
declareDerivedUnit "Mile"    [t| Furlong  |] 8           (Just "mi")
declareDerivedUnit "League"  [t| Mile     |] 3           (Just "lea")

-- | Includes all the lengths above.
surveyLengths :: [Name]
surveyLengths = [ ''Foot, ''Link, ''Rod, ''Chain, ''Furlong
                , ''Mile, ''League ]

declareDerivedUnit "Acre"     [t| Foot :^ Two |] 43560 (Just "acre")
declareDerivedUnit "Section"  [t| Acre |]        640   (Just "section")
declareDerivedUnit "Township" [t| Section |]     36    (Just "twp")

-- | Includes all the areas above.
surveyAreas :: [Name]
surveyAreas = [ ''Acre, ''Section, ''Township ]
