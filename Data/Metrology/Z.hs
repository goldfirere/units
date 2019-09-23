{- Data/Metrology/Z.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   rae@cs.brynmawr.edu

   This file contains a definition of integers at the type-level, in terms
   of a promoted datatype 'Z'.
-}

{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances,
             GADTs, PolyKinds, TemplateHaskell, ScopedTypeVariables,
             EmptyCase, CPP, TypeSynonymInstances, FlexibleInstances #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TypeApplications #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Z
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines a datatype and operations to represent type-level
-- integers. Though it's defined as part of the units package, it may be
-- useful beyond dimensional analysis. If you have a compelling non-units
-- use of this package, please let me (Richard, @rae@ at @cs.brynmawr.edu@)
-- know.
-----------------------------------------------------------------------------

-- allow compilation even without Cabal
#ifndef MIN_VERSION_singletons
#define MIN_VERSION_singletons(a,b,c) 1
#endif

module Data.Metrology.Z (
  -- * The 'Z' datatype
  Z(..),
#if MIN_VERSION_singletons(2,6,0)
  Sing,
#else
  Sing(..),
#endif
  SZ,

#if MIN_VERSION_singletons(1,0,0)
  -- ** Defunctionalization symbols (these can be ignored)
  ZeroSym0, SSym0, SSym1, PSym0, PSym1,
#endif

  -- * Conversions
  zToInt, szToInt,

  -- * Type-level operations
  -- ** Arithmetic
  Succ, Pred, Negate, type (#+), type (#-), type (#*), type (#/),
  sSucc, sPred, sNegate,

  -- ** Comparisons
  type (Data.Metrology.Z.<), NonNegative,

  -- * Synonyms for certain numbers
  One, Two, Three, Four, Five, MOne, MTwo, MThree, MFour, MFive,
  sZero, sOne, sTwo, sThree, sFour, sFive, sMOne, sMTwo, sMThree, sMFour, sMFive,

  -- * Deprecated synonyms
  pZero, pOne, pTwo, pThree, pFour, pFive, pMOne, pMTwo, pMThree, pMFour, pMFive,
  pSucc, pPred
  ) where

import Data.Singletons.TH
import GHC.Exts ( Constraint )

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
type family Negate (z :: Z) :: Z where
  Negate Zero = Zero
  Negate (S z) = P (Negate z)
  Negate (P z) = S (Negate z)

-- | Divide two integers
type family (a :: Z) #/ (b :: Z) :: Z where
  Zero #/ b      = Zero
  a    #/ (P b') = Negate (a #/ (Negate (P b')))
  a    #/ b      = ZDiv b b a

-- | Helper function for division
type family ZDiv (counter :: Z) (n :: Z) (z :: Z) :: Z where
  ZDiv One n (S z')        = S (z' #/ n)
  ZDiv One n (P z')        = P (z' #/ n)
  ZDiv (S count') n (S z') = ZDiv count' n z'
  ZDiv (S count') n (P z') = ZDiv count' n z'

-- | Less-than comparison
type family (a :: Z) < (b :: Z) :: Bool where
  -- fully qualify everywhere, because Data.Singletons.TH started exporting <
  -- at some point
  Zero  Data.Metrology.Z.< Zero   = False
  Zero  Data.Metrology.Z.< (S n)  = True
  Zero  Data.Metrology.Z.< (P n)  = False
  (S n) Data.Metrology.Z.< Zero   = False
  (S n) Data.Metrology.Z.< (S n') = n Data.Metrology.Z.< n'
  (S n) Data.Metrology.Z.< (P n') = False
  (P n) Data.Metrology.Z.< Zero   = True
  (P n) Data.Metrology.Z.< (S n') = True
  (P n) Data.Metrology.Z.< (P n') = n Data.Metrology.Z.< n'

-- | Check if a type-level integer is in fact a natural number
type family NonNegative z :: Constraint where
  NonNegative Zero  = ()
  NonNegative (S z) = ()

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
sZero  = SZero
sOne   = SS sZero
sTwo   = SS sOne
sThree = SS sTwo
sFour  = SS sThree
sFive  = SS sFour

sMOne   = SP sZero
sMTwo   = SP sMOne
sMThree = SP sMTwo
sMFour  = SP sMThree
sMFive  = SP sMFour

-- | Add one to a singleton @Z@.
sSucc :: Sing z -> Sing (Succ z)
sSucc SZero   = sOne
sSucc (SS z') = SS (SS z')
sSucc (SP z') = z'

-- | Subtract one from a singleton @Z@.
sPred :: Sing z -> Sing (Pred z)
sPred SZero   = sMOne
sPred (SS z') = z'
sPred (SP z') = SP (SP z')

-- | Negate a singleton @Z@.
sNegate :: Sing z -> Sing (Negate z)
sNegate SZero = SZero
sNegate (SS z') = SP (sNegate z')
sNegate (SP z') = SS (sNegate z')

-- | Convert a singleton @Z@ to an @Int@.
szToInt :: Sing (z :: Z) -> Int
szToInt = zToInt . fromSing

{-# DEPRECATED pZero, pOne, pTwo, pThree, pFour, pFive, pMOne, pMTwo, pMThree, pMFour, pMFive, pSucc, pPred "The singleton prefix is changing from 'p' to 's'. The 'p' versions will be removed in a future release." #-}

pZero  = sZero
pOne   = sOne
pTwo   = sTwo
pThree = sThree
pFour  = sFour
pFive  = sFive

pMOne   = sMOne
pMTwo   = sMTwo
pMThree = sMThree
pMFour  = sMFour
pMFive  = sMFive

pSucc = sSucc
pPred = sPred
