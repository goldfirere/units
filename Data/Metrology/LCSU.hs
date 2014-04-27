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
  LCSU(DefaultLCSU), DefaultUnitOfDim,
  Lookup, LookupList, MkLCSU
  ) where

import Data.Metrology.Factor
import Data.Metrology.Z

import Data.Singletons.Maybe

data LCSU star = MkLCSU_ [star]
               | DefaultLCSU

type family Lookup (dim :: *) (lcsu :: LCSU *) :: * where
  Lookup dim (MkLCSU_ ((dim, unit) ': rest)) = unit
  Lookup dim (MkLCSU_ ((other, u)  ': rest)) = Lookup dim (MkLCSU_ rest)
  Lookup dim DefaultLCSU                     = DefaultUnitOfDim dim

type family LookupList (keys :: [Factor *]) (map :: LCSU *) :: [Factor *] where
  LookupList '[] lcsu = '[]
  LookupList (F dim z ': rest) lcsu
    = F (Lookup dim lcsu) z ': LookupList rest lcsu

-- | Assign a default unit for a dimension. Necessary only when using
-- default LCSUs.
type family DefaultUnitOfDim (dim :: *) :: *

-- use type family to prevent pattern-matching
type family MkLCSU pairs where
  MkLCSU pairs = MkLCSU_ pairs
