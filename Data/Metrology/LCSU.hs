{- Data/Metrology/LCSU.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   Defines a locally coherent system of units (LCSUs),
   implemented as an association list.
   An LCSU is a from base dimensions to units, thus 
   defining a uniquely mapping units for any dimensions.
-}

{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Data.Metrology.LCSU (
  LCSU(DefaultLCSU), DefaultLCSUUnit,
  Lookup, LookupList, MkLCSU
  ) where

import Data.Metrology.Factor
import Data.Metrology.Z

import Data.Singletons.Maybe

data LCSU star = MkLCSU_ [star]
               | DefaultLCSU

data DefaultLCSUUnit

type family Lookup (key :: *) (map :: [*]) :: * where
  Lookup key ((key, value) ': rest) =  value
  Lookup key (other ': rest)        = Lookup key rest

type family LookupList (keys :: [Factor *]) (map :: LCSU *) :: [Factor *] where
  LookupList '[] lcsu = '[]
  LookupList (F dim z ': rest) (MkLCSU_ lcsu)
    = F (Lookup dim lcsu) z ': LookupList rest (MkLCSU_ lcsu)
  LookupList dims DefaultLCSU = '[F DefaultLCSUUnit Zero]

-- use type family to prevent pattern-matching
type family MkLCSU pairs where
  MkLCSU pairs = MkLCSU_ pairs
