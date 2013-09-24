{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE GADTs, PolyKinds, KindSignatures, RankNTypes, ScopedTypeVariables #-}

module Data.Dimensions.Zahl where

import GHC.TypeLits hiding ((+)(..),(-)(..))
import qualified GHC.TypeLits as Nat
import Data.Dimensions.TypePrelude

-- | The datatype for type-level integers
data Zahl = Posi Nat | Nega Nat

-- | Singleton for Zahl
data instance Sing (z :: Zahl) where
  SPosi :: forall (n :: Nat). Sing n -> Sing (Posi n) 
  SNega :: forall (n :: Nat). Sing n -> Sing (Nega n)
-- you don't nee forall n, it's inferred
  
instance forall (n::Nat) . (SingI n) => SingI (Posi n) where
  sing = SPosi (sing :: Sing n)
  
instance forall (n::Nat) . (SingI n) => SingI (Nega n) where
  sing = SNega (sing :: Sing n)

instance SingE (KindParam :: KindIs Zahl) where
  type DemoteRep (KindParam :: KindIs Zahl) = Integer
  fromSing (SPosi n) = fromSing n
  fromSing (SNega n) = negate $ fromSing n



type family Succ (z :: Zahl) :: Zahl where
  Succ (Posi n) = Posi (n Nat.+ 1)
  Succ (Nega 0) = Posi 1
  Succ (Nega 1) = Posi 0
  Succ (Nega n) = Nega (n Nat.- 1)

type family Pred (z :: Zahl) :: Zahl where
  Pred (Nega n) = Nega (n Nat.+ 1)
  Pred (Posi 0) = Nega 0
  Pred (Posi n) = Posi (n Nat.- 1)

type family Negate (z :: Zahl) :: Zahl where
  Negate (Nega n) = Posi n
  Negate (Posi 0) = Posi 0
  Negate (Posi n) = Nega n

type family (a :: Zahl) + (b :: Zahl) :: Zahl where
  Posi n + Posi m = Posi (n Nat.+ m)
  Nega n + Nega m = Nega (n Nat.+ m)
  Posi n + Nega m = If (m <=? n) (Posi (n Nat.- m)) (Nega (m Nat.- n))
  Nega m + Posi n = If (m <=? n) (Posi (n Nat.- m)) (Nega (m Nat.- n))
  
type family (a :: Zahl) - (b :: Zahl) :: Zahl where
  Posi n - Nega m = Posi (n Nat.+ m)
  Nega n - Posi m = Nega (n Nat.+ m)
  Posi n - Posi m = If (m <=? n) (Posi (n Nat.- m)) (Nega (m Nat.- n))
  Nega m - Nega n = If (m <=? n) (Posi (n Nat.- m)) (Nega (m Nat.- n))



