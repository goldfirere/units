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


