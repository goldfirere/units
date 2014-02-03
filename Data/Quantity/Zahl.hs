{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE GADTs, PolyKinds, KindSignatures, ScopedTypeVariables, TemplateHaskell #-}

module Data.Quantity.Zahl where

import GHC.TypeLits hiding ((+)(..),(-)(..))
import qualified GHC.TypeLits as Nat
import Data.Singletons 
import Data.Singletons.Bool
import Data.Singletons.Eq
import Data.Singletons.TH

import Prelude hiding ((==))

-- | The datatype for type-level integers.
--   In order to produce most readable error messages, we use
--   typelevel natural numbers from 'GHC.TyqpeLits' and extend it 
--   to integers.
--   .
--   The drawback of this approch is having duplicated definitions of zero
--   'Posi' 0 and 'Nega' 0. We try to minimize the bad effect of duplicated
--   zeroes by two means; (1) Library functions always produce @Posi 0@ and
--   (2) Type-level quality '==' is defined so that 'Posi' 0 == 'Nega' 0.


-- | The datatype for type-level integers
data Zahl = Posi Nat | Nega Nat

type Zero = Posi 0


type family Succ (z :: Zahl) :: Zahl where
  Succ (Posi n) = Posi (n Nat.+ 1)
  Succ (Nega 0) = Posi 1
  Succ (Nega 1) = Posi 0
  Succ (Nega n) = Nega (n Nat.- 1)

type family Pred (z :: Zahl) :: Zahl where
  Pred (Nega n) = Nega (n Nat.+ 1)
  Pred (Posi 0) = Nega 1
  Pred (Posi n) = Posi (n Nat.- 1)

type family Negate (z :: Zahl) :: Zahl where
  Negate (Nega n) = Posi n
  Negate (Posi 0) = Posi 0
  Negate (Posi n) = Nega n

type family (a :: Zahl) :+ (b :: Zahl) :: Zahl where
  Posi n :+ Posi m = Posi (n Nat.+ m)
  Nega n :+ Nega m = Nega (n Nat.+ m)
  Nega n :+ Posi m = Posi m :+ Nega n
  Posi 0 :+ Nega 0 = Posi 0
  Posi 0 :+ Nega n = Nega n
  Posi n :+ Nega 0 = Posi n
  Posi n :+ Nega m = If (m <=? n) (Posi (n Nat.- m)) (Nega (m Nat.- n))
  
type family (a :: Zahl) :- (b :: Zahl) :: Zahl where
  Posi n :- Nega m = Posi n :+ Posi m
  Nega n :- Posi m = Nega n :+ Nega m
  Posi n :- Posi m = Posi n :+ Nega m
  Nega n :- Nega m = Posi m :+ Nega n


type family EqZahl a b where
  EqZahl (Posi a) (Posi a) = True
  EqZahl (Nega a) (Nega a) = True
  EqZahl (Posi 0) (Nega 0) = True
  EqZahl (Nega 0) (Posi 0) = True
  EqZahl a        b        = False

type instance a == b = EqZahl a b



$( promoteOnly [d| 
  
                isZero :: Zahl -> Bool             
                isZero (Posi 0) = True           
                isZero (Nega 0) = True           
                isZero _        = False
               
 |] )

