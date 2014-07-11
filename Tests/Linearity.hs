{- Test the property of quantity as linear space
   Copyright (c) 2014 Richard Eisenberg
-}

module Tests.Linearity where

import Data.Metrology.Poly
import Data.Metrology.Show ()
import Data.Metrology.SI.Poly

import Test.Tasty
import Test.Tasty.HUnit

len1 :: Length SI Rational
len1 = 1 % Meter

linearCompose :: (Fractional a) => a -> a -> Qu d l a  -> Qu d l a -> Qu d l a 
linearCompose a b x y = a *| x |+| b *| y


tests :: TestTree
tests = testGroup "Show"
  [ testCase "Identity" $ len1 @?= len1
  , testCase "Addition" $ len1 |+| len1 @?= 2 *| len1 
  , testCase "Summation of multiple quantities" $ 
      qSum [len1,len1,len1] @?= 3 *| len1 
  , testCase "Linear composition" $ 
      linearCompose 0.2 0.8 len1 len1 @?= len1
  ]

