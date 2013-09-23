{- Data/Dimensions/Map.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg 
   eir@cis.upenn.edu

   This file defines the type-level Map and operations over them
-}

{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, TypeOperators, 
    UndecidableInstances #-}

module Data.Dimensions.Map where

import Data.Dimensions.TypePrelude

type Map a b = '[ '(a, b)]

-- | @(Extract s map)@ pulls a key-value pair 
--   that matches s out of lst, returning a
--   diminished map and, possibly, the extracted key.
--
-- @
-- Extract A [(A,1), (B,2)] ==> ([(B,2)], Just (A,1)
-- Extract D [(A,1), (B,2)] ==> ([(A,1), (B,2)], Nothing)
-- @
type family Extract (s :: a)
                    (map :: [(a, b)] )
                 :: ([(a, b)], Maybe (a,b)) where
  Extract s '[] = '( '[], Nothing )

infix 4 :==:
-- | Type-level equality over any kind.
type family (a :: k) :==: (b :: k) :: Bool where
  (a :: k) :==: (a :: k) = True
  (a :: k) :==: (b :: k) = False
