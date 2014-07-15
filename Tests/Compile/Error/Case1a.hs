{-# LANGUAGE TypeOperators,TypeFamilies #-}

import Data.Metrology.Poly
import Data.Metrology.Show
import Data.Metrology.SI.Poly

type QofU u = MkQu_ULN u SI Double

data Jansky = Jansky
instance Show Jansky where show _ = "Jy"

instance Unit Jansky where
  type BaseUnit Jansky = (Kilo :@ Gram) :/ (Second :^ Two) 
  conversionRatio _ = 1e-26

ratio :: Double -> QofU Number
ratio x = area1 x -- (area1 x) is used as wrong dimension here. However type error is reported not here

len0 :: QofU Meter 
len0 = 2.5 % Meter

area0 :: QofU (Meter :^ Two)
area0 = len0 |^ sTwo -- but here.

area1 :: Double -> QofU (Meter :^ Two)
area1 x = x *| (4.27 % (Meter :^ sTwo))


measureOfSensitivity :: Int -> QofU Number
measureOfSensitivity n = modelNorm n -- modelNorm n is used in wrong dimension here

modelNorm :: Int -> QofU (Jansky :^ Two) 
modelNorm n = modelNorm' n % (Jansky :^ sTwo)  

modelNorm' :: Int -> Double
modelNorm' 1 = 1.23
modelNorm' 2 = 4.56

main :: IO ()
main = print area1

{-
$ ghc Case1a.hs
[1 of 1] Compiling Main             ( Case1a.hs, Case1a.o )

Case1a.hs:23:9:
    Couldn't match type ‘'F
                           Data.Metrology.SI.Dims.Length ('S 'Zero #* 'S ('S 'Zero))
                           : ('[] @* 'S ('S 'Zero))’
                  with ‘'[]’
    Expected type: QofU (Meter :^ Two)
      Actual type: Qu
                     ('['F Data.Metrology.SI.Dims.Length One] @* 'S ('S 'Zero))
                     SI
                     Double
    In the expression: len0 |^ sTwo
    In an equation for ‘area0’: area0 = len0 |^ sTwo

Case1a.hs:33:15:
    Couldn't match type ‘'F
                           Data.Metrology.SI.Dims.Mass ('S 'Zero #* 'S One)
                           : ('['F Data.Metrology.SI.Dims.Time ('P ('P 'Zero))] @* 'S One)’
                  with ‘'[]’
    Expected type: QofU (Jansky :^ Two)
      Actual type: Qu '[] SI Double
    In the expression: modelNorm' n % (Jansky :^ sTwo)
    In an equation for ‘modelNorm’:
        modelNorm n = modelNorm' n % (Jansky :^ sTwo)

Case1a.hs:33:28:
    Couldn't match type ‘'[]’
                  with ‘'['F Data.Metrology.SI.Dims.Mass ('S One),
                          'F Data.Metrology.SI.Dims.Time ('P ('P ('P ('P 'Zero))))]’
    In the expression: modelNorm' n % (Jansky :^ sTwo)
    In an equation for ‘modelNorm’:
        modelNorm n = modelNorm' n % (Jansky :^ sTwo)

-}