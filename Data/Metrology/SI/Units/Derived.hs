{-# LANGUAGE TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.SI.Units.Derived
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports derived unit definitions according to the SI
-- system of units. These are, according with
-- <http://www.bipm.org/en/si/si_brochure/chapter4/table6.html this link>
-- "Non-SI units accepted for use with the SI,
-- and units based on fundamental constants".
--
-----------------------------------------------------------------------------

module Data.Metrology.SI.Units.Derived where

import Data.Metrology
import Data.Metrology.SI
{-import Data.Metrology.SI.Dims-}

-- | 1 min = 60 s
data Minute = Minute
instance Unit Minute where
  type BaseUnit Minute = Second
  conversionRatio _ = 60
instance Show Minute where
  show _ = "min"

-- | 1 hour = 60 min = 3600 s
data Hour = Hour
instance Unit Hour where
  type BaseUnit Hour = Second
  conversionRatio _ = 60 * 60
instance Show Hour where
  show _ = "hour"

-- | 1 day = 24 h = 86400 s
data Day = Day
instance Unit Day where
  type BaseUnit Day = Second
  conversionRatio _ = 24 * 60 * 60
instance Show Day where
  show _ = "day"



