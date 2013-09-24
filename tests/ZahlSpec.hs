{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE GADTs, PolyKinds, KindSignatures, RankNTypes, ScopedTypeVariables #-}

module ZahlSpec (spec) where

import GHC.TypeLits hiding ((+)(..), (-)(..))
import Test.Hspec

import Data.Dimensions.Zahl


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
    
    