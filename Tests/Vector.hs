{-# LANGUAGE TypeFamilies #-}

{- Test the property of vector quantity 
   Copyright (c) 2014 Richard Eisenberg
-}

module Tests.Vector where

import Data.Metrology.Vector
import Data.Metrology.Show ()
import Data.Metrology.SI.Poly
import Data.VectorSpace

import Test.Tasty
import Test.Tasty.HUnit

len1 :: Length SI Rational
len1 = 1 % Meter


type V2 = Length SI (Rational, Rational)    
vec1 :: V2
vec1 = (2,3) % Meter

linearCompose :: (VectorSpace n, a ~ Scalar n) => a -> a -> Qu d l n  -> Qu d l n -> Qu d l n
linearCompose a b x y = a *| x |+| b *| y


tests :: TestTree
tests = testGroup "Show"
  [ testCase "Identity" $ vec1 @?= vec1
  , testCase "Addition" $ vec1 |+| vec1 @?= 2 *| vec1 
  , testCase "Summation of multiple quantities" $ 
      qSum [vec1,vec1,vec1] @?= 3 *| vec1 
  , testCase "Linear composition" $ 
      linearCompose 0.2 0.8 vec1 vec1 @?= vec1

  , testCase "Multiplication from right" $ 4 *| vec1 @?= vec1 |* 4
  , testCase "Division by scalar" $ 10 *| vec1 @?= vec1 |/ 0.1

  , testCase "scalar product" $
      (5 % Meter) |*^| vec1 @?= ((10, 15) % (Meter :^ sTwo))
  ]

