{-# LANGUAGE DataKinds, FlexibleInstances, FlexibleContexts, KindSignatures, MultiParamTypeClasses, TypeFamilies #-}
module Data.Quantity.Quantity where

import Data.Quantity.System
import Data.Quantity.Map
import Data.Quantity.Zahl

newtype Qu (lcsu :: [(DimK *, UnitK *)]) (unit :: [(UnitK *, Zahl)])  (value :: *) = Qu value
  deriving (Eq, Show)

(|+|) :: Num val => Qu lcsu unit val -> Qu lcsu unit' val -> Qu lcsu unit val 
(Qu x) |+| (Qu y) = Qu (x+y)

(|-|) :: Num val => Qu lcsu unit val -> Qu lcsu unit' val -> Qu lcsu unit val 
(Qu x) |-| (Qu y) = Qu (x-y)


(|*|) :: Num val => Qu lcsu unit val -> Qu lcsu unit' val -> Qu lcsu (AddMap unit unit') val 
(Qu x) |*| (Qu y) = Qu (x*y)

