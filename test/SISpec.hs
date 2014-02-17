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



type LengthDim =  '[ '( Dim Length , Posi 1) ] 
type SpeedDim =  '[ '( Dim Length , Posi 1) ,  '( Dim Time , Nega 1) ] 
type TimeDim =  '[ '( Dim Time , Posi 1) ]  

type SpeedDim2 =  '[  '( Dim Time , Nega 1),  '( Dim Length , Posi 1) ,  '( Dim Current , Nega 0) ] 

type Fib = '[ Posi 1, Posi 1, Posi 2, Posi 3, Posi 5 ]

type SI =   '[ '( Dim Length , Uni Meter) , '(Dim Time, Uni Second)] 

marathonLength :: Qu SI '[ '(Uni Meter, Posi 1) ] Rational
marathonLength = Qu 42195

marathonLengthInKm :: Qu SI '[ '(Uni KiloMeter, Posi 1) ] Rational
marathonLengthInKm = convertUnit $ marathonLength

spec :: Spec 
spec = do
  describe "Quantity calculus library" $ do
    it "can lookup typelevel map correctly" $
      fromSing (sing :: Sing ((Lookup (Dim Length) SpeedDim) :== ('Just (Posi 1))))  
    it "can convert to numerical value correctly" $
      (toNumerical $ marathonLength) `shouldBe` 42195
    it "can convert between units correctly" $
      (toNumerical $ marathonLengthInKm) `shouldBe` 42.195      