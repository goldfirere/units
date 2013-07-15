{- Data/Dimensions/TypePrelude.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   Type-level prelude-like operations.
   
   Note to self: Consider using the type-prelude package instead.
-}

{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, TypeOperators #-}

module Data.Dimensions.TypePrelude where

-- | Extract the first element of a pair
type family Fst (x :: (a,b)) :: a
type instance Fst '(a,b) = a

-- | Extract the second element of a pair
type family Snd (x :: (a,b)) :: b
type instance Snd '(a,b) = b

-- | Type-level conditional
type family If (switch :: Bool) (true :: k) (false :: k) :: k where
  If True  t f = t
  If False t f = f

infixr 3 :&&:
-- | Type-level "and"
type family (a :: Bool) :&&: (b :: Bool) :: Bool where
  False :&&: a = False
  True  :&&: a = a

infixr 2 :||:
-- | Type-level "or"
type family (a :: Bool) :||: (b :: Bool) :: Bool where
  False :||: a = a
  True  :||: a = True

infix 4 :=:
-- | Type-level equality over @*@.
type family (a :: *) :=: (b :: *) :: Bool where
  (a :: *) :=: (a :: *) = True
  (a :: *) :=: (b :: *) = False
