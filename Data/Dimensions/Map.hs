{- Data/Dimensions/Map.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   Defines a type-level Map, implemented as an association list.
-}

{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Data.Dimensions.Map (
  Map,
  Lookup, LookupList, MkLCSU
  ) where

import Data.Dimensions.DimSpec

import Data.Singletons.Maybe

data Map star = MkM [star]

type family Lookup (key :: *) (map :: Map *) :: * where
  Lookup key (MkM ((key, value) ': rest)) =  value
  Lookup key (MkM (other ': rest))         = Lookup key (MkM rest)

type family LookupList (keys :: [DimSpec *]) (map :: Map *) :: [DimSpec *] where
  LookupList '[] lcsu = '[]
  LookupList (D dim z ': rest) lcsu = D (Lookup dim lcsu) z ': LookupList rest lcsu

-- use type family to prevent pattern-matching
type family MkLCSU pairs where
  MkLCSU pairs = MkM pairs