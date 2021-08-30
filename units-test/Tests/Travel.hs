{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}

module Tests.Travel where

import Data.Metrology.Poly
import Data.Metrology.SI.Poly
-- import Data.Metrology.Imperial.Types (Imperial)
-- import Data.Metrology.Imperial.Units
import Data.Units.US (Gallon(..), Mile(..), Pound(..), Yard)
import Data.Metrology.Show ()
import qualified Data.Dimensions.SI as D

import Test.Tasty
import Test.Tasty.HUnit
import Test.HUnit.Approx

type PerArea lcsu n = MkQu_DLN (D.Area :^ MOne) lcsu n

-- import the Imperial type from version 1.1 of 
-- unit-defs/Data.Metrology.Imperial.Types
--
-- The old version seems to have had 1 Gallon be 0.00454609 m^3,
-- but version 2.0 of units-defs has 1 Gallon be 231 inch^3
--
--   231 inch^3 = 231 * (0.0254^3) m^3
--              = 0.00378542
--
-- and
--
--   0.00454609 / 0.00378542 = 1.200947
--
-- so I use this value to scale the answers below
--

gallonChange :: Float
gallonChange = 231 * (0.0254 ** 3) / 0.00454609  

type Imperial =
  MkLCSU
   '[ (D.Length, Yard)
    , (D.Mass, Pound)
    , (D.Time, Second)
    {- Not needed by these tests
    , (D.Current, Ampere)
    , (D.Temperature, Kelvin)
    , (D.AmountOfSubstance, Mole)
    , (D.LuminousIntensity, Candela)
    -}
   ]

fromGLtoED :: MkQu_DLN D.Length Imperial Float
fromGLtoED = 46.5 % Mile

fuelEfficiency :: PerArea Imperial Float
fuelEfficiency = 40 % (Mile :/ Gallon)

gasolineDensity :: MkQu_DLN D.Density Imperial Float
gasolineDensity = 7.29 % (Pound :/ Gallon)

gasolineWeight :: (Fractional f) 
  => MkQu_DLN D.Length su f -> PerArea su f -> MkQu_DLN D.Density su f -> MkQu_DLN D.Mass su f
gasolineWeight len0 ef0 den0 = len0 |/| ef0 |*| den0

tests :: TestTree
tests =
  let ?epsilon = 0.00001 in
  testGroup "Travel"
  [ testCase "fromGLtoED" (fromGLtoED # Mile @?~ 46.5)
  , testCase "fuelEfficiency" (fuelEfficiency # (Mile :/ Gallon) @?~ 39.999996)
  , testCase "gasolineDensity" (gasolineDensity # (Pound :/ Gallon) @?~ 7.29)
  , testCase "gasolineWeight" (gasolineWeight fromGLtoED fuelEfficiency gasolineDensity # Pound @?~ 8.474626)
  , testCase "fromGLtoED2" (fromGLtoED # kilo Meter @?~ 74.834496)
  , testCase "fuelEfficiency2" (fuelEfficiency # (kilo Meter :/ Liter) @?~ (14.160248 / gallonChange))
  , testCase "gasolineDensity2" (gasolineDensity # (kilo Gram :/ Liter) @?~ (0.7273698 / gallonChange))
  , testCase "gasolineWeight2" ((gasolineWeight (convert fromGLtoED) (convert fuelEfficiency) (convert gasolineDensity) :: MkQu_DLN D.Mass SI Float) # kilo Gram @?~ 3.8440251) ]

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
    (convert fromGLtoED) (convert fuelEfficiency) (convert gasolineDensity) :: MkQu_DLN D.Mass SI Float)

{---- Execution result ---
46.5 mi
39.999996 mi/gal
7.29 lb/gal
8.474626 lb

74.834496 km
14.160248 km/l
0.7273698 kg/l
3.8440251 kg
-}
