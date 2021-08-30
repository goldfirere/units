{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.Imperial.Weight
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Due to the <https://en.wikipedia.org/wiki/International_yard_and_pound
-- International yard and pound agreement of 1959>, it so happens that
-- Imperial and US customary units of weight have the same base (the
-- international pound at 0.45359237 kilograms). However, if subdivisions
-- are generally the same, Imperial units feature different multiples of
-- the pound, due to the presence of the stone. As in, using English
-- conventions, the hundredweight is the *long* one (the short one is the
-- cental). As is the ton.
-- Also, of the troy system (the international one being, more or less,
-- the avoirdupoids one) only the ounce is legal.
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

module Data.Units.Imperial.Weight where

import Data.Metrology.TH

import qualified Data.Units.SI as SI
import qualified Data.Units.US.Avoirdupois as Avdp

import Language.Haskell.TH

declareDerivedUnit "Pound"         [t| Avdp.Pound    |] 1          (Just "lb")
declareDerivedUnit "Grain"         [t| Avdp.Grain    |] 1          (Just "gr")
declareDerivedUnit "Dram"          [t| Avdp.Dram     |] 1          (Just "dr")
declareDerivedUnit "Ounce"         [t| Avdp.Ounce    |] 1          (Just "oz")
declareDerivedUnit "TroyOunce"     [t| SI.Gramme     |] 31.1034768 (Just "t oz")
declareDerivedUnit "Stone"         [t| Pound         |] 14         (Just "st")
declareDerivedUnit "Quarter"       [t| Stone         |] 2          (Just "qr")
declareDerivedUnit "Cental"        [t| Pound         |] 100        (Just "cental")
declareDerivedUnit "Hundredweight" [t| Quarter       |] 4          (Just "cwt")
declareDerivedUnit "Ton"           [t| Hundredweight |] 20         (Just "t")

-- | Includes 'Ounce', 'Pound', 'Ton'
commonMassMeasures :: [Name]
commonMassMeasures = [ ''Ounce, ''Pound, ''Ton ]

-- | Includes 'Grain', 'Dram', 'TroyOunce', 'Stone', 'Quarter', 'Cental' and 'Hundredweight'
otherMassMeasures :: [Name]
otherMassMeasures = [ ''Grain, ''Dram, ''TroyOunce, ''Stone, ''Quarter, ''Cental, ''Hundredweight ]
