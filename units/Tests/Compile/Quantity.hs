{- Test Quantity instance
   Copyright (c) 2014 Richard Eisenberg
-}

{-# LANGUAGE DataKinds, TypeFamilies, UndecidableInstances #-}

module Tests.Compile.Quantity where

import Data.Metrology.Poly
import Data.Metrology.Quantity
import Data.Metrology.SI.Poly
import Data.Metrology.SI   ()  -- DefaultLCSU instances

len1 :: Length SI Double
len1 = 5 % Meter

len2 :: Length SI Double
len2 = fromQuantity len1

len3 :: Length SI Double
len3 = toQuantity len1

force1, force2, force3 :: Energy DefaultLCSU Double
force1 = 10 % Joule
force2 = fromQuantity force1
force3 = toQuantity force1

newtype MyTime = MyTime { getTime :: Double }   -- measured in seconds

instance Quantity MyTime where
  type QuantityUnit MyTime = Second
  type QuantityLCSU MyTime = SI

  fromQuantity = MyTime . (# Second)
  toQuantity = (% Second) . getTime
