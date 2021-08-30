{-# LANGUAGE TypeOperators,TypeFamilies #-}

import Data.Metrology.Poly
import Data.Metrology.Show
import Data.Metrology.SI.Poly

data Jansky = Jansky
instance Show Jansky where show _ = "Jy"

instance Unit Jansky where
  type BaseUnit Jansky = (Kilo :@ Gram) :/ (Second :^ Two) 
  conversionRatio _ = 1e-26

ratio :: Double -> MkQu_ULN Number SI Double
ratio x = area1 x -- (area1 x) is used as wrong dimension here. However type error is reported not here

len0 ::  MkQu_ULN Meter SI Double
len0 = 2.5 % Meter

area0 ::  MkQu_ULN (Meter :^ Two) SI Double
area0 = len0 |^ sTwo -- but here.

area1 :: Double ->  MkQu_ULN (Meter :^ Two) SI Double
area1 x = x *| (4.27 % (Meter :^ sTwo))


measureOfSensitivity :: Int ->  MkQu_ULN Number SI Double
measureOfSensitivity n = modelNorm n -- modelNorm n is used in wrong dimension here

modelNorm :: Int ->  MkQu_ULN (Jansky :^ Two)  SI Double
modelNorm n = modelNorm' n % (Jansky :^ sTwo)  

modelNorm' :: Int -> Double
modelNorm' 1 = 1.23
modelNorm' 2 = 4.56

main :: IO ()
main = print area1

{-
$ ghc Case1c.hs
[1 of 1] Compiling Main             ( Case1c.hs, Case1c.o )

Case1c.hs:21:9:
    Couldn't match type ‘'F
                           Data.Metrology.SI.Dims.Length ('S 'Zero #* 'S ('S 'Zero))
                           : ('[] @* 'S ('S 'Zero))’
                  with ‘'[]’
    Expected type: MkQu_ULN (Meter :^ Two) SI Double
      Actual type: Qu
                     ('['F Data.Metrology.SI.Dims.Length One] @* 'S ('S 'Zero))
                     SI
                     Double
    In the expression: len0 |^ sTwo
    In an equation for ‘area0’: area0 = len0 |^ sTwo

Case1c.hs:31:15:
    Couldn't match type ‘'F
                           Data.Metrology.SI.Dims.Mass ('S 'Zero #* 'S One)
                           : ('['F Data.Metrology.SI.Dims.Time ('P ('P 'Zero))] @* 'S One)’
                  with ‘'[]’
    Expected type: MkQu_ULN (Jansky :^ Two) SI Double
      Actual type: Qu '[] SI Double
    In the expression: modelNorm' n % (Jansky :^ sTwo)
    In an equation for ‘modelNorm’:
        modelNorm n = modelNorm' n % (Jansky :^ sTwo)

Case1c.hs:31:28:
    Couldn't match type ‘'[]’
                  with ‘'['F Data.Metrology.SI.Dims.Mass ('S One),
                          'F Data.Metrology.SI.Dims.Time ('P ('P ('P ('P 'Zero))))]’
    In the expression: modelNorm' n % (Jansky :^ sTwo)
    In an equation for ‘modelNorm’:
        modelNorm n = modelNorm' n % (Jansky :^ sTwo)
-}