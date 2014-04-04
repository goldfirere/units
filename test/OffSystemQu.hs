{-# LANGUAGE EmptyCase, FlexibleContexts, TemplateHaskell, PolyKinds, DataKinds, TypeFamilies, ScopedTypeVariables, GADTs, StandaloneDeriving, RankNTypes, TypeOperators, UndecidableInstances #-}

module Test.OffSystemQu where

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

data Foot = Foot
data instance Sing Foot = SFoot
instance SingI Foot where sing = SFoot
instance IsUnitName Foot where
  type DimOfUnitName Foot = '[ '(Dim Length, Posi 1) ]
  conversionFactorOfName _ =  0.3048

type YP =  
  '[ '( Dim Length , Uni Foot) ]

(%) :: (IsUnit '[ '( Uni u , Posi 1) ],
        IsUnit (CoherentUnit (Qu l  '[ '( Uni u , Posi 1) ] Double )))
    => Double -> u -> Qu l  '[ '( Uni u , Posi 1) ] Double
x % u = fromNumerical x

m1 :: Qu SI '[ '(Uni Meter, Posi 1) ] Double
m1 = (1 % Meter) |+| (3 % Foot)