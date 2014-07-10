{- Test the property of quantity as vector space
   Copyright (c) 2014 Richard Eisenberg
-}

module Tests.Vector where

import Data.Metrology.Poly
import Data.Metrology.Show ()
import Data.Metrology.SI.Poly

import Test.Tasty
import Test.Tasty.HUnit

len1 :: Length SI Rational
len1 = 1 % Meter


{-

Since the vector space with units are as follows:

    -- | Multiply a quantity by a scalar from the right
    (|*) :: VectorSpace n => Qu a l n -> Scalar n -> Qu a l n
    (Qu a) |* b = Qu (a ^* b)

An example of 2d vector with unit signature will be as follows:

    type V2 = Length SI (Rational, Rational)    
    coord1 :: V2
    coord1 = (2,3) % Meter

But then we encounter erros like follows:

    Tests/Vector.hs:20:16:
        No instance for (Fractional (Rational, Rational))
          arising from a use of ‘%’
        In the expression: (2, 3) % Meter
        In an equation for ‘coord1’: coord1 = (2, 3) % Meter

-}


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

