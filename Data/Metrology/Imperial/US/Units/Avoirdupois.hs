{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Imperial.US.Units.Avoirdupois
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines avoirdupois measures of mass. The avoirdupois
-- system is the one most commonly used in the US.
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

module Data.Metrology.Imperial.US.Units.Avoirdupois where

import Data.Metrology
import Data.Metrology.TH
import Data.Metrology.SI.Units ( Gram )

import Language.Haskell.TH

declareDerivedUnit "Pound"             [t| Gram  |] 453.59237    (Just "lb")

declareDerivedUnit "Grain"             [t| Pound |] (1/7000)     (Just "gr")
declareDerivedUnit "Dram"              [t| Grain |] (27 + 11/32) (Just "dr")
declareDerivedUnit "Ounce"             [t| Pound |] (1/16)       (Just "oz")
declareDerivedUnit "Hundredweight"     [t| Pound |] 100          (Just "cwt")
declareDerivedUnit "LongHundredweight" [t| Pound |] 112          (Just "longcwt")
declareDerivedUnit "Ton"               [t| Pound |] 2000         (Just "ton")
declareDerivedUnit "LongTon"           [t| Pound |] 2240         (Just "longton")

commonMassMeasures :: [Name]
commonMassMeasures = [ ''Ounce, ''Pound, ''Ton]

otherMassMeasures :: [Name]
otherMassMeasures = [ ''Grain, ''Dram, ''Hundredweight, ''LongHundredweight
                    , ''LongTon ]
