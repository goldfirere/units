{-# LANGUAGE TypeOperators, DataKinds, TypeFamilies, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Tests.OffSystemCSU where

import Data.Metrology.Poly
import Data.Metrology.SI
import qualified Data.Dimensions.SI as D

import Test.Tasty.HUnit
import Test.HUnit.Approx

type YardPond = MkLCSU '[ (D.Length, Foot)]

type LengthYP = MkQu_DLN D.Length YardPond Double

data Foot = Foot
instance Unit Foot where
  type BaseUnit Foot = Meter
  conversionRatio _ = 0.3048

data Year = Year
instance Unit Year where
  type BaseUnit Year = Second
  conversionRatio _ = 60 * 60 * 24 * 365.242

len1 :: LengthYP
len1 = 3 % Foot 

len2 :: LengthYP
len2 = 1 % Meter

x :: Double
x = (len1 |+| len2) # Meter

tests = testCase "OffSystemCSU" (x @?~ 1.9144)
