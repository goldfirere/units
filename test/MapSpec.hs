{-# LANGUAGE EmptyCase, TemplateHaskell, PolyKinds, DataKinds, TypeFamilies, ScopedTypeVariables, GADTs, StandaloneDeriving, RankNTypes, TypeOperators, UndecidableInstances #-}

module MapSpec where

import Data.Singletons
import Data.Singletons.Bool
import Data.Singletons.Tuple
import Data.Singletons.TH
import Data.Quantity.Dimension
import Data.Quantity.Dimension.SI
import Data.Quantity.Map
import Data.Quantity.Zahl

import Test.Hspec



type LengthDim = FromList '[ '( BaseDim Length , Posi 1) ] 
type SpeedDim = FromList '[ '( BaseDim Length , Posi 1) ,  '( BaseDim Time , Nega 1) ] 
type TimeDim = FromList '[ '( BaseDim Time , Posi 1) ]  


spec :: Spec 
spec = do
  describe "Type level Map library" $ do
    it "can lookup typelevel map correctly" $ do
      fromSing (sing :: Sing ((Lookup (BaseDim Length) SpeedDim) :== ('Just (Posi 1))))  
        `shouldBe` True
      fromSing (sing :: Sing ((Lookup (BaseDim Length) TimeDim) :== ('Nothing)))
        `shouldBe` True        