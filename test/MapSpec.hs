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



type LengthDim =  '[ '( BaseDim Length , Posi 1) ] 
type SpeedDim =  '[ '( BaseDim Length , Posi 1) ,  '( BaseDim Time , Nega 1) ] 
type TimeDim =  '[ '( BaseDim Time , Posi 1) ]  

type SpeedDim2 =  '[  '( BaseDim Time , Nega 1),  '( BaseDim Length , Posi 1) ,  '( BaseDim Current , Nega 0) ] 

type Fib = '[ Posi 1, Posi 1, Posi 2, Posi 3, Posi 5 ]

spec :: Spec 
spec = do
  describe "Type level Map library" $ do
    it "can lookup typelevel map correctly" $
      fromSing (sing :: Sing ((Lookup (BaseDim Length) SpeedDim) :== ('Just (Posi 1))))  
    it "detects absence correctly" $                         
      fromSing (sing :: Sing ((Lookup (BaseDim Length) TimeDim) :== ('Nothing)))

    it "uniqs correctly" $                         
      fromSing (sing :: Sing (Uniq '[Posi 1, Posi 2, Posi 1] :== '[Posi 1, Posi 2]))

    it "equates correctly" $                         
      fromSing (sing :: Sing (EqMap SpeedDim SpeedDim2))
