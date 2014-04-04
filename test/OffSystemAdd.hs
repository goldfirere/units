{-# LANGUAGE TypeFamilies #-}

module Test.OffSystemAdd where

import Data.Dimensions
import Data.Dimensions.SI

data Foot = Foot
instance Unit Foot where
  type BaseUnit Foot = Meter
  conversionRatio _ = 0.3048

data Year = Year
instance Unit Year where
  type BaseUnit Year = Second
  conversionRatio _ = 60 * 60 * 24 * 365.242

vel1 :: Velocity
vel1 = 1e6 % (Foot :/ Year)

vel2 :: Velocity
vel2 = 0.01 % (Meter :/ Second)


vel1InMS :: Double
vel1InMS = vel1 # (Meter :/ Second)

vel2InMS :: Double
vel2InMS = vel2 # (Meter :/ Second)

vel12InMS :: Double
vel12InMS = (vel1 .+ vel2) # (Meter :/ Second)


len1 :: Length
len1 = 3 % Foot

len2 :: Length
len2 = 1 % Meter

len12InM :: Double
len12InM = (len1 .+ len2) # Meter
