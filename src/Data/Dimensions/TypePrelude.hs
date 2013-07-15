{- Data/Dimensions/TypePrelude.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   Type-level prelude-like operations.
   
   Note to self: Consider using the type-prelude package instead.
-}

{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, TypeOperators #-}

module Data.Dimensions.TypePrelude where

type family Fst (x :: (a,b)) :: a
type instance Fst '(a,b) = a

type family Snd (x :: (a,b)) :: b
type instance Snd '(a,b) = b

type family If (switch :: Bool) (true :: k) (false :: k) :: k where
  If True  t f = t
  If False t f = f

infixr 3 :&&:
type family (a :: Bool) :&&: (b :: Bool) :: Bool where
  False :&&: a = False
  True  :&&: a = a

infixr 2 :||:
type family (a :: Bool) :||: (b :: Bool) :: Bool where
  False :||: a = a
  True  :||: a = True

-- Type-level equality over *.
infix 4 :=:
type family (a :: k) :=: (b :: k) :: Bool where
  (a :: *) :=: (a :: *) = True
  (a :: *) :=: (b :: *) = False
