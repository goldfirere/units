{-# LANGUAGE EmptyCase, TemplateHaskell, PolyKinds, DataKinds, TypeFamilies, ScopedTypeVariables, GADTs, StandaloneDeriving, RankNTypes, TypeOperators, UndecidableInstances #-}

module SISpec where

import Data.Singletons
import Data.Singletons.Bool
import Data.Singletons.Tuple
import Data.Singletons.TH

import Data.Quantity.Map
import Data.Quantity.Quantity
import Data.Quantity.System
import Data.Quantity.System.SI
import Data.Quantity.Zahl

import Test.Hspec


type SI =  
  '[ '( Dim Length , Uni Meter) 
  , '(Dim Time, Uni Second) 
  , '(Dim Mass, Uni (Kilo Gram))] 

type CGS =  
  '[ '( Dim Length , Uni (Centi Meter)) 
  , '(Dim Time, Uni Second) 
  , '(Dim Mass, Uni Gram)] 


marathonLength :: Qu SI '[ '(Uni Meter, Posi 1) ] Rational
marathonLength = Qu 42195

marathonLengthInKm :: Qu SI '[ '(Uni (Kilo Meter), Posi 1) ] Rational
marathonLengthInKm = convertUnit $ marathonLength

waterDensity :: Qu CGS '[ '(Uni Gram, Posi 1) , '(Uni (Centi Meter), Nega 3) ] Rational
waterDensity = Qu 1

waterDensityInSI :: Qu SI '[ '(Uni (Kilo Gram), Posi 1) , '(Uni Meter, Nega 3) ] Rational
waterDensityInSI = convertCSU waterDensity



spec :: Spec 
spec = do
  describe "Quantity calculus library" $ do
    it "can convert to numerical value correctly" $
      (toNumerical $ marathonLength) `shouldBe` 42195
    it "can convert between units correctly" $
      (toNumerical $ marathonLengthInKm) `shouldBe` 42.195      
      
    it "can convert between CSU correctly" $
      (toNumerical $ waterDensityInSI) `shouldBe` 1000