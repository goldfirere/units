-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.Imperial.Nautical
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The nautical mile defined in this module is the old Admiralty one. A
-- difference with the old definition is that we use here the old
-- definition of 6080 feet.
--
-- Where possible, reference have been made to UK legislation. However,
-- Wikipedia's base is /much/ better organized than any government
-- resource immediately available.
-----------------------------------------------------------------------------

module Data.Units.Imperial.Nautical where

import Data.Metrology.TH

import Data.Units.Imperial.Length

import Language.Haskell.TH

declareDerivedUnit "Fathom"       [t| Cable        |] 0.01 (Just "ftm")
declareDerivedUnit "Cable"        [t| NauticalMile |] 0.1  (Just "cable")
declareDerivedUnit "NauticalMile" [t| Foot         |] 6080 (Just "NM")

-- | Nautical lengths: All units above
nauticalLengths :: [Name]
nauticalLengths = [ ''Fathom, ''Cable, ''NauticalMile ]
