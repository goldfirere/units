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
ratio x = area1 x -- (area1 x) is used as wrong dimension here

len0 :: QofU Meter 
len0 = 2.5 % Meter

area0 :: QofU (Meter :^ Two)
area0 = len0 |^ sTwo

area1 :: Double -> QofU (Meter :^ Two)
area1 x = x *| (4.27 % (Meter :^ sTwo))


-- measureOfSensitivity :: Int -> QofU Number
-- measureOfSensitivity n = modelNorm n -- modelNorm n is used in wrong dimension here
-- 
-- modelNorm :: Int -> QofU (Jansky :^ Two) 
-- modelNorm n = modelNorm' n % (Jansky :^ sTwo)  
-- 
-- modelNorm' :: Int -> Double
-- modelNorm' 1 = 1.23
-- modelNorm' 2 = 4.56

main :: IO ()
main = print area1

{-
$ ghc Case1b.hs
[1 of 1] Compiling Main             ( Case1b.hs, Case1b.o )

Case1b.hs:17:11:
    Couldn't match type ‘'[]’
                  with ‘'['F Data.Metrology.SI.Dims.Length ('S ('S 'Zero))]’
    Expected type: QofU Number
      Actual type: QofU (Meter :^ Two)
    In the expression: area1 x
    In an equation for ‘ratio’: ratio x = area1 x
-}