{- Data/Dimensions/Z.hs
 
   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file contains a definition of integers at the type-level, in terms
   of a promoted datatype 'Z'.
-}

{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances,
             GADTs, PolyKinds #-}

module Data.Dimensions.Z where

import GHC.TypeLits ( Sing, SingI(..), SingE(..), KindIs(..) )

data Z = Zero | S Z | P Z

zToInt :: Z -> Int
zToInt Zero = 0
zToInt (S z) = zToInt z + 1
zToInt (P z) = zToInt z - 1

type family Succ (z :: Z) :: Z where
  Succ Zero = S Zero
  Succ (P z) = z
  Succ (S z) = S (S z)

type family Pred (z :: Z) :: Z where
  Pred Zero = P Zero
  Pred (P z) = P (P z)
  Pred (S z) = z

infixl 6 #+
type family (a :: Z) #+ (b :: Z) :: Z where
  Zero   #+ z      = z
  (S z1) #+ (S z2) = S (S (z1 #+ z2))
  (S z1) #+ Zero   = S z1
  (S z1) #+ (P z2) = z1 #+ z2
  (P z1) #+ (S z2) = z1 #+ z2
  (P z1) #+ Zero   = P z1
  (P z1) #+ (P z2) = P (P (z1 #+ z2))

infixl 6 #-
type family (a :: Z) #- (b :: Z) :: Z where
  z      #- Zero = z
  (S z1) #- (S z2) = z1 #- z2
  Zero   #- (S z2) = P (Zero #- z2)
  (P z1) #- (S z2) = P (P (z1 #- z2))
  (S z1) #- (P z2) = S (S (z1 #- z2))
  Zero   #- (P z2) = S (Zero #- z2)
  (P z1) #- (P z2) = z1 #- z2

infixl 7 #*
type family (a :: Z) #* (b :: Z) :: Z where
  Zero #* z = Zero
  (S z1) #* z2 = (z1 #* z2) #+ z2
  (P z1) #* z2 = (z1 #* z2) #- z2

type family NegZ (z :: Z) :: Z where
  NegZ Zero = Zero
  NegZ (S z) = P (NegZ z)
  NegZ (P z) = S (NegZ z)

type family (a :: Z) #/ (b :: Z) :: Z where
  Zero #/ b      = Zero
  a    #/ (P b') = NegZ (a #/ (NegZ (P b')))
  a    #/ b      = ZDiv b b a

type family ZDiv (counter :: Z) (n :: Z) (z :: Z) :: Z where
  ZDiv One n (S z')        = S (z' #/ n)
  ZDiv One n (P z')        = P (z' #/ n)
  ZDiv (S count') n (S z') = ZDiv count' n z'
  ZDiv (S count') n (P z') = ZDiv count' n z'

type family (a :: Z) < (b :: Z) :: Bool where
  Zero  < Zero   = False
  Zero  < (S n)  = True
  Zero  < (P n)  = False
  (S n) < Zero   = False
  (S n) < (S n') = n < n'
  (S n) < (P n') = False
  (P n) < Zero   = True
  (P n) < (S n') = True
  (P n) < (P n') = n < n'

type One   = S Zero
type Two   = S One
type Three = S Two
type Four  = S Three
type Five  = S Four

type MOne   = P Zero
type MTwo   = P MOne
type MThree = P MTwo
type MFour  = P MThree
type MFive  = P MFour

---- Singleton for Z
data instance Sing (z :: Z) where
  SZero :: Sing Zero
  SS    :: Sing z -> Sing (S z)
  SP    :: Sing z -> Sing (P z)

instance SingI Zero where
  sing = SZero
instance SingI z => SingI (S z) where
  sing = SS sing
instance SingI z => SingI (P z) where
  sing = SP sing

instance SingE (KindParam :: KindIs Z) where
  type DemoteRep (KindParam :: KindIs Z) = Z
  fromSing SZero  = Zero
  fromSing (SS z) = S (fromSing z)
  fromSing (SP z) = P (fromSing z)

pZero  = SZero
pOne   = SS pZero
pTwo   = SS pOne
pThree = SS pTwo
pFour  = SS pThree
pFive  = SS pFour

pMOne   = SP pZero
pMTwo   = SP pMOne
pMThree = SP pMTwo
pMFour  = SP pMThree
pMFive  = SP pMFour

pSucc :: Sing z -> Sing (Succ z)
pSucc SZero   = pOne
pSucc (SS z') = SS (SS z')
pSucc (SP z') = z'

pPred :: Sing z -> Sing (Pred z)
pPred SZero   = pMOne
pPred (SS z') = z'
pPred (SP z') = SP (SP z')

szToInt :: Sing (z :: Z) -> Int
szToInt = zToInt . fromSing
