{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.US.Troy
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines troy measures of mass. The troy
-- system is most often used when measuring precious metals.
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

module Data.Units.US.Troy (
  module Data.Units.US.Troy,

  -- | The avoirdupois grain is the same as the troy grain
  Grain(..)
  ) where

import Data.Metrology.TH
import Data.Units.US.Avoirdupois ( Grain(..) )

import Language.Haskell.TH

declareDerivedUnit "Pennyweight" [t| Grain       |] 24 (Just "dwt")
declareDerivedUnit "Ounce"       [t| Pennyweight |] 20 (Just "ozt")
declareDerivedUnit "Pound"       [t| Ounce       |] 12 (Just "lbt")

-- | Includes 'Grain', 'Pennyweight', 'Ounce', and 'Pound'
troyMassMeasures :: [Name]
troyMassMeasures = [ ''Grain, ''Pennyweight, ''Ounce, ''Pound ]
