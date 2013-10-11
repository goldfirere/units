{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators #-}
{-# LANGUAGE PolyKinds, KindSignatures, UndecidableInstances #-}

module ZahlSpec (spec) where

import Control.Applicative
import GHC.TypeLits hiding ((+)(..), (-)(..))
import System.IO
import System.IO.Unsafe
import System.Process (runInteractiveCommand)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Text.Printf
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


expr2Type :: Expr -> String
expr2Type PZ = "(Posi 0)"
expr2Type NZ = "(Nega 0)"
expr2Type (Val x) 
  | x >= 0    = printf "(Posi %d)" x
  | otherwise = printf "(Nega %d)" (negate x)
expr2Type (Add x y) = printf "(%s + %s)" (expr2Type x) (expr2Type y)
expr2Type (Sub x y) = printf "(%s - %s)" (expr2Type x) (expr2Type y)

expr2Prog :: Expr -> String
expr2Prog expr = unlines
  [ "{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators #-}"         
  , "{-# LANGUAGE PolyKinds, KindSignatures, UndecidableInstances #-}"
  , "import GHC.TypeLits hiding ((+)(..), (-)(..))"
  , "import Data.Dimensions.Zahl"
  , "main :: IO ()"  
  , printf "main = print $ fromSing (sing :: Sing (%s))" (expr2Type expr)
  ]

typeLevelEval :: Expr -> Integer
typeLevelEval expr = unsafePerformIO $ do
  (hIn , hOut, _, _) <- runInteractiveCommand $ "runhaskell -isrc/ " 
  hPutStr hIn $ expr2Prog expr
  hClose hIn
  msg <- hGetContents hOut
  return $ read msg



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
    (eval expr == typeLevelEval expr)