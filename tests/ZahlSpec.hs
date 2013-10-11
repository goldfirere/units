{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE GADTs, PolyKinds, KindSignatures, RankNTypes, ScopedTypeVariables #-}

module ZahlSpec (spec) where

import Control.Applicative
import GHC.TypeLits hiding ((+)(..), (-)(..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen


import Data.Dimensions.Zahl

data Expr 
  = PZ
  | NZ
  | Val Integer
  | Add Expr Expr
  | Sub Expr Expr
    deriving (Eq, Show)

instance Arbitrary Expr where
  arbitrary = 
    let 
      go n
        | n <= 1 = 
           oneof [ return PZ
                 , return NZ ]
        | otherwise = do 
           nL <- choose (0,n)  
           let nR = n - nL
           oneof [ Val <$> arbitrary
                 , Add <$> resize nL arbitrary <*> resize nR arbitrary 
                 , Sub <$> resize nL arbitrary <*> resize nR arbitrary ]
    in sized go 

         
  shrink (Add x y) 
    =  [x,y] 
    ++ (Add <$> shrink x <*> [y]) 
    ++ (Add <$> [x] <*> shrink y)
  shrink (Sub x y) 
    =  [x,y] 
    ++ (Sub <$> shrink x <*> [y]) 
    ++ (Sub <$> [x] <*> shrink y)
  shrink (Val n) = Val <$> shrink n
  shrink _ = []

eval :: Expr -> Integer
eval PZ = 0
eval NZ = 0
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y


eval' :: Expr -> Integer
eval' PZ = 0
eval' NZ = 0
eval' (Sub (Val x) NZ) 
  | x >= 3 = x - 2
  | otherwise = x 
eval' (Val x) = x
eval' (Add x y) = eval x + eval y
eval' (Sub x y) = eval x - eval y

spec :: Spec
spec = describe "Typelevel integer library" $ do
  it "converts to correct values" $ do
    fromSing (sing :: Sing (Posi 42)) `shouldBe` 42
  it "adds two value correctly" $ do
    fromSing (sing :: Sing (Posi 30 + Posi 42)) `shouldBe` 72
    fromSing (sing :: Sing (Posi 30 + Nega 42)) `shouldBe` (-12)
    
  it "subtracts two value correctly" $ do
    fromSing (sing :: Sing (Posi 30 - Posi 42)) `shouldBe` (-12)
    fromSing (sing :: Sing (Posi 30 - Nega 42)) `shouldBe` 72
    
  prop "evaluates correctly" $ \expr ->
    eval expr == eval' expr