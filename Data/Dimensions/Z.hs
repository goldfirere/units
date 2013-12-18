{- Data/Dimensions/Z.hs
 
   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file contains a definition of integers at the type-level, in terms
   of a promoted datatype 'Z'.
-}

{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances,
             GADTs, PolyKinds, TemplateHaskell, ScopedTypeVariables,
             EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | This module defines a datatype and operations to represent type-level
-- integers. Though it's defined as part of the units package, it may be
-- useful beyond dimensional analysis. If you have a compelling non-units
-- use of this package, please let me (Richard, @eir@ at @cis.upenn.edu@)
-- know.

module Data.Dimensions.Z where

import Data.Singletons.TH

-- | The datatype for type-level integers.
$(singletons [d| data Z = Zero | S Z | P Z deriving Eq |])

-- | Convert a 'Z' to an 'Int'
zToInt :: Z -> Int
zToInt Zero = 0
zToInt (S z) = zToInt z + 1
zToInt (P z) = zToInt z - 1

-- | Add one to an integer
type family Succ (z :: Z) :: Z where
  Succ Zero = S Zero
  Succ (P z) = z
  Succ (S z) = S (S z)

-- | Subtract one from an integer
type family Pred (z :: Z) :: Z where
  Pred Zero = P Zero
  Pred (P z) = P (P z)
  Pred (S z) = z

infixl 6 #+
-- | Add two integers
type family (a :: Z) #+ (b :: Z) :: Z where
  Zero   #+ z      = z
  (S z1) #+ (S z2) = S (S (z1 #+ z2))
  (S z1) #+ Zero   = S z1
  (S z1) #+ (P z2) = z1 #+ z2
  (P z1) #+ (S z2) = z1 #+ z2
  (P z1) #+ Zero   = P z1
  (P z1) #+ (P z2) = P (P (z1 #+ z2))

infixl 6 #-
-- | Subtract two integers
type family (a :: Z) #- (b :: Z) :: Z where
  z      #- Zero = z
  (S z1) #- (S z2) = z1 #- z2
  Zero   #- (S z2) = P (Zero #- z2)
  (P z1) #- (S z2) = P (P (z1 #- z2))
  (S z1) #- (P z2) = S (S (z1 #- z2))
  Zero   #- (P z2) = S (Zero #- z2)
  (P z1) #- (P z2) = z1 #- z2

infixl 7 #*
-- | Multiply two integers
type family (a :: Z) #* (b :: Z) :: Z where
  Zero #* z = Zero
  (S z1) #* z2 = (z1 #* z2) #+ z2
  (P z1) #* z2 = (z1 #* z2) #- z2

-- | Negate an integer
type family NegZ (z :: Z) :: Z where
  NegZ Zero = Zero
  NegZ (S z) = P (NegZ z)
  NegZ (P z) = S (NegZ z)

-- | Divide two integers
type family (a :: Z) #/ (b :: Z) :: Z where
  Zero #/ b      = Zero
  a    #/ (P b') = NegZ (a #/ (NegZ (P b')))
  a    #/ b      = ZDiv b b a

-- | Helper function for division
type family ZDiv (counter :: Z) (n :: Z) (z :: Z) :: Z where
  ZDiv One n (S z')        = S (z' #/ n)
  ZDiv One n (P z')        = P (z' #/ n)
  ZDiv (S count') n (S z') = ZDiv count' n z'
  ZDiv (S count') n (P z') = ZDiv count' n z'

-- | Less-than comparison
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

-- | This is the singleton value representing @Zero@ at the term level and
-- at the type level, simultaneously. Used for raising units to powers.
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

-- | Add one to a singleton @Z@.
pSucc :: Sing z -> Sing (Succ z)
pSucc SZero   = pOne
pSucc (SS z') = SS (SS z')
pSucc (SP z') = z'

-- | Subtract one from a singleton @Z@.
pPred :: Sing z -> Sing (Pred z)
pPred SZero   = pMOne
pPred (SS z') = z'
pPred (SP z') = SP (SP z')

-- | Convert a singleton @Z@ to an @Int@.
szToInt :: Sing (z :: Z) -> Int
szToInt = zToInt . fromSing
