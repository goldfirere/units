{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE GADTs, PolyKinds, KindSignatures, ScopedTypeVariables #-}

module Data.Dimensions.Zahl where

import GHC.TypeLits hiding ((+)(..),(-)(..))
import qualified GHC.TypeLits as Nat
import Data.Quantity.Zahl
import Prelude hiding ((==))

-- RAE: This seems wrong: there seems to be 2 representations for 0.
-- No time to fix right now.

-- TM: Thank you for the comment. I documented my approach, and I will
-- experiment if it works out.

-- | The datatype for type-level integers.
--   In order to produce most readable error messages, we use
--   typelevel natural numbers from 'GHC.TypeLits' and extend it 
--   to integers.
--   .
--   The drawback of this approch is having duplicated definitions of zero
--   'Posi' 0 and 'Nega' 0. We try to minimize the bad effect of duplicated
--   zeroes by two means; (1) Library functions always produce @Posi 0@ and
--   (2) Type-level quality '==' is defined so that 'Posi' 0 == 'Nega' 0.


-- | The datatype for type-level integers
data Zahl = Posi Nat | Nega Nat

-- | Singleton for Zahl
data instance Sing (z :: Zahl) where
  SPosi :: Sing n -> Sing (Posi n) 
  SNega :: Sing n -> Sing (Nega n)
  
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
  Pred (Posi 0) = Nega 1
  Pred (Posi n) = Posi (n Nat.- 1)

type family Negate (z :: Zahl) :: Zahl where
  Negate (Nega n) = Posi n
  Negate (Posi 0) = Posi 0
  Negate (Posi n) = Nega n

type family (a :: Zahl) + (b :: Zahl) :: Zahl where
  Posi n + Posi m = Posi (n Nat.+ m)
  Nega n + Nega m = Nega (n Nat.+ m)
  Nega n + Posi m = Posi m + Nega n
  Posi 0 + Nega 0 = Posi 0
  Posi 0 + Nega n = Nega n
  Posi n + Nega 0 = Posi n
  Posi n + Nega m = If (m <=? n) (Posi (n Nat.- m)) (Nega (m Nat.- n))
  
type family (a :: Zahl) - (b :: Zahl) :: Zahl where
  Posi n - Nega m = Posi (n Nat.+ m)
  Nega n - Posi m = Nega (n Nat.+ m)
  Posi n - Posi m = Posi n + Nega m
  Nega n - Nega m = Posi m + Nega n

type family (a :: Zahl) == (b :: Zahl) :: Bool where
  Posi a == Posi a = True
  Nega a == Nega a = True
  Posi 0 == Nega 0 = True
  Nega 0 == Posi 0 = True
  a == b           = False


