{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Imperial.US.Units.Apothecaries
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines apothecaries' measures of mass. These measures
-- are rarely used.
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

module Data.Metrology.Imperial.US.Units.Apothecaries (
  module Data.Metrology.Imperial.US.Units.Apothecaries,

  -- | The apothecaries' grain is the same as the avoirdupois grain.
  Grain(..),

  -- | The apothecaries' ounce and pound are the troy ounce and pound.
  Ounce(..), Pound(..)
  ) where

import Data.Metrology
import Data.Metrology.TH
import Data.Metrology.Imperial.US.Units.Avoirdupois ( Grain(..) )
import Data.Metrology.Imperial.US.Units.Troy ( Ounce(..), Pound(..) )

import Language.Haskell.TH

declareDerivedUnit "Scruple" [t| Grain |] 20   (Just "sap")
declareDerivedUnit "Dram"    [t| Grain |] 60   (Just "drap")

apothecariesMassMeasures :: [Name]
apothecariesMassMeasures = [ ''Grain, ''Scruple, ''Dram, ''Ounce, ''Pound ]
