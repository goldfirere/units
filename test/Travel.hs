{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Test.Travel where

import Data.Dimensions
import Data.Dimensions.SI.Types
import Data.Dimensions.SI.Prefixes
import Data.Dimensions.SI.Units
import Data.Dimensions.SI (SI)
import Data.Dimensions.Imperial.Types (Imperial)
import Data.Dimensions.Imperial.Units
import Data.Dimensions.Show
import qualified Data.Dimensions.SI.Dims as D

type PerArea lcsu n = MkGenDim (D.Area :^ MOne) lcsu n

fromGLtoED :: MkGenDim D.Length Imperial Float
fromGLtoED = 46.5 % Mile

fuelEfficiency :: PerArea Imperial Float
fuelEfficiency = 40 % (Mile :/ Gallon)

gasolineDensity :: MkGenDim D.Density Imperial Float
gasolineDensity = 7.29 % (Pound :/ Gallon)

gasolineWeight :: (Fractional f) 
  => MkGenDim D.Length su f -> PerArea su f -> MkGenDim D.Density su f -> MkGenDim D.Mass su f
gasolineWeight len0 ef0 den0 = len0 ./ ef0 .* den0


main :: IO ()
main = do
  putStrLn $ fromGLtoED `showIn` Mile
  putStrLn $ fuelEfficiency `showIn` Mile :/ Gallon
  putStrLn $ gasolineDensity `showIn` Pound :/ Gallon
  putStrLn $ show $ gasolineWeight fromGLtoED fuelEfficiency gasolineDensity 

  putStrLn ""
  putStrLn $ fromGLtoED `showIn` (kilo Meter)
  putStrLn $ fuelEfficiency `showIn`  kilo Meter :/ Liter
  putStrLn $ gasolineDensity `showIn` kilo Gram :/ Liter
  putStrLn $ show $ (gasolineWeight 
    (convert fromGLtoED) (convert fuelEfficiency) (convert gasolineDensity) :: MkGenDim D.Mass SI Float)

{---- Execution result ---

46.5 mi
40.0 mi/gal
7.29 lb/gal
8.474625 lb

74.834496 km
14.160249 km/l
0.7273698 kg/l
3.844025 kg

-}
