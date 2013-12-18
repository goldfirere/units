{- Data/Dimensions/Z.hs
 
   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file contains a definition of integers at the type-level, in terms
   of a promoted datatype 'Z'.
-}

{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances,
             GADTs, PolyKinds, TemplateHaskell, ScopedTypeVariables,
             EmptyCase, ConstraintKinds #-}

-- | This module defines a datatype and operations to represent type-level
-- integers. Though it's defined as part of the units package, it may be
-- useful beyond dimensional analysis. If you have a compelling non-units
-- use of this package, please let me (Richard, @eir@ at @cis.upenn.edu@)
-- know.

module Data.Dimensions.Z where

import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality
import Data.Singletons
import Unsafe.Coerce

data Z = Pos Nat
       | Neg Nat       -- this Nat should never be 0!

-- | Singleton definition for integers
data instance Sing (z :: Z) where
  SPos :: Sing n -> Sing (Pos n)
  SNeg :: Sing n -> Sing (Neg n)

instance SingI n => SingI (Pos n) where
  sing = SPos sing
instance SingI n => SingI (Neg n) where
  sing = SNeg sing

instance SingKind ('KProxy :: KProxy Z) where
  type DemoteRep ('KProxy :: KProxy Z) = Integer

  fromSing (SPos n) = fromSing n
  fromSing (SNeg n) = negate $ fromSing n

  toSing n
    | n >= 0    = withSomeSing   n  (\n' -> SomeSing $ SPos n')
    | otherwise = withSomeSing (-n) (\n' -> SomeSing $ SNeg n')

-- | Add one to an integer
type family Succ z where
  Succ (Pos n) = Pos (n + 1)
  Succ (Neg 1) = Pos 0
  Succ (Neg n) = Neg (n - 1)

-- | Subtract one from an integer
type family Pred z where
  Pred (Neg n) = Neg (n + 1)
  Pred (Pos 0) = Neg 1
  Pred (Pos n) = Pos (n - 1)

infixl 6 #+
-- | Add two integers
type family a #+ b where
  (Pos a) #+ (Pos b) = Pos (a + b)
  (Pos a) #+ (Neg b) = If (b <=? a) (Pos (a - b)) (Neg (b - a))
  (Neg b) #+ (Pos a) = If (b <=? a) (Pos (a - b)) (Neg (b - a))
  (Neg a) #+ (Neg b) = Neg (a + b)

infixl 6 #-
-- | Subtract two integers
type family a #- b where
  (Pos a) #- (Pos b) = If (b <=? a) (Pos (a - b)) (Neg (b - a))
  (Pos a) #- (Neg b) = Pos (a + b)
  (Neg a) #- (Pos b) = Neg (a + b)
  (Neg b) #- (Neg a) = If (b <=? a) (Pos (a - b)) (Neg (b - a))

infixl 7 #*
-- | Multiply two integers
type family a #* b where
  (Pos 0) #* x       = Pos 0
  x       #* (Pos 0) = Pos 0
  (Pos a) #* (Pos b) = Pos (a * b)
  (Pos a) #* (Neg b) = Neg (a * b)
  (Neg a) #* (Pos b) = Neg (a * b)
  (Neg a) #* (Neg b) = Pos (a * b)

-- | Negate an integer
type family NegateZ z where
  NegateZ (Pos 0) = Pos 0
  NegateZ (Neg n) = Pos n
  NegateZ (Pos n) = Neg n

-- | Divide two integers
type family a #/ b where
  (Pos 0) #/ x       = Pos 0
  x       #/ (Pos 0) = Error "divide by 0"
  (Pos a) #/ (Pos b) = Pos (NatDiv a b)
  (Pos a) #/ (Neg b) = Neg (NatDiv a b)
  (Neg a) #/ (Pos b) = Neg (NatDiv a b)
  (Neg a) #/ (Neg b) = Pos (NatDiv a b)

-- can't use If here, because type families are *eager*!
type family NatDiv num denom where
  NatDiv num denom = NatDivHelper (denom <=? num) num denom

type family NatDivHelper lt num denom where
  NatDivHelper True  num denom = 1 + NatDiv (num - denom) denom
  NatDivHelper False num denom = 0

-- | Boolean less-than on Z
type family a #< b where
  (Neg a) #< (Pos b) = True
  (Pos a) #< (Neg b) = False
  (Pos a) #< (Pos b) = (a <=? b) && (Not (a == b))
  (Neg a) #< (Neg b) = (b <=? a) && (Not (a == b))

-- | This is the singleton value representing @Zero@ at the term level and
-- at the type level, simultaneously. Used for raising units to powers.
pZero  = SPos (sing :: Sing 0)
pOne   = SPos (sing :: Sing 1)
pTwo   = SPos (sing :: Sing 2)
pThree = SPos (sing :: Sing 3)
pFour  = SPos (sing :: Sing 4)
pFive  = SPos (sing :: Sing 5)

pMOne   = SNeg (sing :: Sing 1)
pMTwo   = SNeg (sing :: Sing 2)
pMThree = SNeg (sing :: Sing 3)
pMFour  = SNeg (sing :: Sing 4)
pMFive  = SNeg (sing :: Sing 5)

data Dict c where
  Dict :: c => Dict c

magic :: Sing n -> Dict (KnownNat n)
magic = undefined

-- | Add one to a singleton @Z@.
pSucc :: Sing z -> Sing (Succ z)
pSucc (SPos n) = SPos (unsafeCoerce (fromSing n + 1))


{-
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

-}