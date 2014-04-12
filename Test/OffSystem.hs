{-# LANGUAGE TypeFamilies #-}

module Test.OffSystem where

import Data.Metrology
import Data.Metrology.SI
import Data.Metrology.DimSpec
import Data.Metrology.Units
import Data.Metrology.LCSU

data Foot = Foot
instance Unit Foot where
  type BaseUnit Foot = Meter
  conversionRatio _ = 0.3048

data Year = Year
instance Unit Year where
  type BaseUnit Year = Second
  conversionRatio _ = 60 * 60 * 24 * 365.242

vel :: Velocity
vel = 1e6 % (Foot :/ Year)

velInMS :: Double
velInMS = vel # (Meter :/ Second)