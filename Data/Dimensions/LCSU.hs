{- Data/Dimensions/LCSU.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   Defines a locally-consistent system of units,
   implemented as an association list.
-}

{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Data.Dimensions.LCSU (
  LCSU(DefaultLCSU), DefaultLCSUUnit,
  Lookup, LookupList, MkLCSU
  ) where

import Data.Dimensions.DimSpec
import Data.Dimensions.Z

import Data.Singletons.Maybe

data LCSU star = MkLCSU_ [star]
               | DefaultLCSU

data DefaultLCSUUnit

type family Lookup (key :: *) (map :: [*]) :: * where
  Lookup key ((key, value) ': rest) =  value
  Lookup key (other ': rest)        = Lookup key rest

type family LookupList (keys :: [DimSpec *]) (map :: LCSU *) :: [DimSpec *] where
  LookupList '[] lcsu = '[]
  LookupList (D dim z ': rest) (MkLCSU_ lcsu)
    = D (Lookup dim lcsu) z ': LookupList rest (MkLCSU_ lcsu)
  LookupList dims DefaultLCSU = '[D DefaultLCSUUnit Zero]

-- use type family to prevent pattern-matching
type family MkLCSU pairs where
  MkLCSU pairs = MkLCSU_ pairs
