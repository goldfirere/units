{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Imperial.Units
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports unit definitions according to the British Imperial system of units.
-- The definitions were taken from here: <http://en.wikipedia.org/wiki/Imperial_units>.
--
-----------------------------------------------------------------------------

module Data.Metrology.Imperial.Units where

import Data.Metrology
import Data.Metrology.SI.Units (Meter(..), Gram(..))

---------------------------------
-- Lengths
---------------------------------

data Thou = Thou
instance Unit Thou where
  type BaseUnit Thou = Inch
  conversionRatio _ = 1/1000
instance Show Thou where
  show _ = "th"

data Inch = Inch
instance Unit Inch where
  type BaseUnit Inch = Foot
  conversionRatio _ = 1/12
instance Show Inch where
  show _ = "in"

data Foot = Foot
instance Unit Foot where
  type BaseUnit Foot = Yard
  conversionRatio _ = 1/3
instance Show Foot where
  show _ = "ft"

data Yard = Yard
instance Unit Yard where
  type BaseUnit Yard = Meter
  conversionRatio _ = 0.9144
instance Show Yard where
  show _ = "yd"

data Chain = Chain
instance Unit Chain where
  type BaseUnit Chain = Yard
  conversionRatio _ = 22
instance Show Chain where
  show _ = "ch"

data Furlong = Furlong
instance Unit Furlong where
  type BaseUnit Furlong = Chain
  conversionRatio _ = 10
instance Show Furlong where
  show _ = "fur"

data Mile = Mile
instance Unit Mile where
  type BaseUnit Mile = Furlong
  conversionRatio _ = 8
instance Show Mile where
  show _ = "mi"

data League = League
instance Unit League where
  type BaseUnit League = Mile
  conversionRatio _ = 3
instance Show League where
  show _ = "lea"

---------------------------------
-- Volumes
---------------------------------

data Gallon = Gallon
instance Unit Gallon where
  type BaseUnit Gallon = (Meter :^ Three)
  conversionRatio _ = 0.00454609
instance Show Gallon where
  show _ = "gal"

---------------------------------
-- Weights
---------------------------------

data Ounce = Ounce
instance Unit Ounce where
  type BaseUnit Ounce = Pound
  conversionRatio _ = 1/16
instance Show Ounce where
  show _ = "oz"

data Pound = Pound
instance Unit Pound where
  type BaseUnit Pound = Gram
  conversionRatio _ = 453.59237    -- on Earth, at least!
instance Show Pound where
  show _ = "lb"



