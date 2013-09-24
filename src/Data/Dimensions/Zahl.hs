{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE GADTs, PolyKinds, KindSignatures #-}

module Data.Dimensions.Zahl where

import GHC.TypeLits hiding ((+)(..),(-)(..))
import qualified GHC.TypeLits as Nat
import Data.Dimensions.TypePrelude

-- | The datatype for type-level integers
data Zahl = Plus Nat | Minus Nat

-- | Singleton for Zahl
data

type family Succ (z :: Zahl) :: Zahl where
  Succ (Plus n) = Plus (n Nat.+ 1)
  Succ (Minus 0) = Plus 1
  Succ (Minus 1) = Plus 0
  Succ (Minus n) = Minus (n Nat.- 1)

type family Pred (z :: Zahl) :: Zahl where
  Pred (Minus n) = Minus (n Nat.+ 1)
  Pred (Plus 0) = Minus 0
  Pred (Plus n) = Plus (n Nat.- 1)

type family Negate (z :: Zahl) :: Zahl where
  Negate (Minus n) = Plus n
  Negate (Plus 0) = Plus 0
  Negate (Plus n) = Minus n

type family (a :: Zahl) + (b :: Zahl) :: Zahl where
  Plus n + Plus m = Plus (n Nat.+ m)
  Minus n + Minus m = Minus (n Nat.+ m)
  Plus n + Minus m = If (m <=? n) (Plus (n Nat.- m)) (Minus (m Nat.- n))
  Minus m + Plus n = If (m <=? n) (Plus (n Nat.- m)) (Minus (m Nat.- n))
  
type family (a :: Zahl) - (b :: Zahl) :: Zahl where
  Plus n - Minus m = Plus (n Nat.+ m)
  Minus n - Plus m = Minus (n Nat.+ m)
  Plus n - Plus m = If (m <=? n) (Plus (n Nat.- m)) (Minus (m Nat.- n))
  Minus m - Minus n = If (m <=? n) (Plus (n Nat.- m)) (Minus (m Nat.- n))



