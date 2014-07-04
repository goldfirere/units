{- Test Show instances
   Copyright (c) 2014 Richard Eisenberg
-}

module Tests.Show where

import Data.Metrology
import Data.Metrology.Show ()
import Data.Metrology.SI

import Test.Tasty
import Test.Tasty.HUnit

five :: Double
five = 5

tests :: TestTree
tests = testGroup "Show"
  [ testCase "Meter" $ show (five % Meter) @?= "5.0 m"
  , testCase "Meter/Second" $ show (five % (Meter :/ Second)) @?= "5.0 m/s"
  , testCase "Meter/Second2" $ show (five % (Number :* Second :* Meter :/ (Second :^ sTwo))) @?= "5.0 m/s"
  , testCase "Hertz" $ show (five % Hertz) @?= "5.0 s^-1"
  , testCase "Joule" $ show (five % Joule) @?= "5.0 (kg * m^2)/s^2"
  ]
