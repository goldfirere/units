{- Data/Metrology/Set.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   Defines set-like operations on type-level lists.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Set
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines a few set-like operations on type-level lists. It
-- may be applicable beyond the units package.
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module Data.Metrology.Set where

import GHC.Exts ( Constraint )

-- | Are two lists equal, when considered as sets?
type family SetEqual (as :: [k]) (bs :: [k]) :: Constraint where
  SetEqual as bs = (Subset as bs, Subset bs as)

-- | Is one list a subset of the other?
type family Subset (as :: [k]) (bs :: [k]) :: Constraint where
  Subset '[] bs = (() :: Constraint)
  Subset (a ': as) bs = (a `Elem` bs, as `Subset` bs)

-- | Is an element contained in a list?
type family Elem (a :: k) (bs :: [k]) :: Constraint where
  Elem a (a ': bs) = (() :: Constraint)
  Elem a (b ': bs) = a `Elem` bs
