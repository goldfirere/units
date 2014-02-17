{-# LANGUAGE DataKinds, FlexibleInstances, FlexibleContexts, KindSignatures, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies #-}
module Data.Quantity.Quantity where

import Data.Singletons

import Data.Quantity.System
import Data.Quantity.Map
import Data.Quantity.Zahl

newtype Qu (lcsu :: [(DimK *, UniK *)]) (unit :: [(UniK *, Zahl)])  (value :: *) = Qu value
  deriving (Eq, Show)

type family CoherentUnit q :: [(UniK *, Zahl)]where
  CoherentUnit (Qu l u v) = LookupLCSU l u


(|+|) :: (Num val, EqMap (DimOf unit) (DimOf unit') ~ True) => Qu lcsu unit val -> Qu lcsu unit' val -> Qu lcsu unit val 
(Qu x) |+| (Qu y) = Qu (x+y)

(|-|) :: (Num val, EqMap (DimOf unit) (DimOf unit') ~ True) => Qu lcsu unit val -> Qu lcsu unit' val -> Qu lcsu unit val 
(Qu x) |-| (Qu y) = Qu (x-y)

(|*|) :: Num val => Qu lcsu unit val -> Qu lcsu unit' val -> Qu lcsu (AddMap unit unit') val 
(Qu x) |*| (Qu y) = Qu (x*y)


convertUnit :: (EqMap (DimOf unit) (DimOf unit') ~ True) => Qu lcsu unit val -> Qu lcsu unit' val 
convertUnit (Qu x) = Qu x



toNumerical :: forall l u v. (Fractional v, IsUnit u, IsUnit (CoherentUnit (Qu l u v))) => Qu l u v -> v
toNumerical (Qu x) = 
  fromRational cf * x
  where
    cf = conversionFactor (undefined :: Sing (CoherentUnit (Qu l u v)))
       / conversionFactor (undefined :: Sing u)
         
fromNumerical :: forall l u v. (Fractional v, IsUnit u, IsUnit (CoherentUnit (Qu l u v))) => v -> Qu l u v 
fromNumerical x = 
  Qu (fromRational cf * x)
  where
    cf = conversionFactor (undefined :: Sing u)
       / conversionFactor (undefined :: Sing (CoherentUnit (Qu l u v)))
