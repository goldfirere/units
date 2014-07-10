{- Test The property of quantity as vector space
   Copyright (c) 2014 Richard Eisenberg
-}

module Tests.Vector where

import Data.Metrology
import Data.Metrology.Show ()
import Data.Metrology.SI

import Test.Tasty
import Test.Tasty.HUnit

x :: Length
x = 1 % Meter

linearCompose :: (Fractional a) => a -> a -> Qu d l a  -> Qu d l a -> Qu d l a 
linearCompose a b x y = a *| x |+| b *| y


tests :: TestTree
tests = testGroup "Show"
  [ testCase "Meter" $ x @?= x
  , testCase "Meter" $ 2 *| x @?= x |+| x  
  , testCase "Meter" $ 3 *| x @?= qSum [x,x,x]  
  ]

