{-# LANGUAGE EmptyCase, TemplateHaskell, PolyKinds, DataKinds, TypeFamilies, ScopedTypeVariables, GADTs, StandaloneDeriving, RankNTypes, TypeOperators, UndecidableInstances #-}

module MapSpec where

import Data.Singletons
import Data.Singletons.Bool
import Data.Singletons.Tuple
import Data.Singletons.TH
import Data.Quantity.System
import Data.Quantity.System.SI
import Data.Quantity.Map
import Data.Quantity.Zahl

import Test.Hspec



type LengthQu =  '[ '( Qu Length , Posi 1) ] 
type SpeedQu =  '[ '( Qu Length , Posi 1) ,  '( Qu Time , Nega 1) ] 
type TimeQu =  '[ '( Qu Time , Posi 1) ]  

type SpeedDim2 =  '[  '( Qu Time , Nega 1),  '( Qu Length , Posi 1) ,  '( Qu Current , Nega 0) ] 

type Fib = '[ Posi 1, Posi 1, Posi 2, Posi 3, Posi 5 ]

spec :: Spec 
spec = do
  describe "Type level Map library" $ do
    it "can lookup typelevel map correctly" $
      fromSing (sing :: Sing ((Lookup (Qu Length) SpeedDim) :== ('Just (Posi 1))))  
    it "detects absence correctly" $                         
      fromSing (sing :: Sing ((Lookup (Qu Length) TimeDim) :== ('Nothing)))

    it "uniqs correctly" $                         
      fromSing (sing :: Sing (Uniq '[Posi 1, Posi 2, Posi 1] :== '[Posi 1, Posi 2]))

    it "equates correctly" $                         
      fromSing (sing :: Sing (EqMap SpeedQu SpeedDim2))
