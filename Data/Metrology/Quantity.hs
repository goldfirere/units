-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Quantity
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports a class 'Quantity' to allow easy conversion between proper
-- quantities and types from other libraries.
------------------------------------------------------------------------------

{-# LANGUAGE DataKinds, TypeFamilies, ConstraintKinds, UndecidableInstances #-}

module Data.Metrology.Quantity where

import Data.Metrology.Poly

class Quantity t where
  -- | The unit associated with @t@.
  type QuantityUnit t :: *

  -- | The LCSU associated with @t@. Defaults to 'DefaultLCSU'.
  type QuantityLCSU t :: LCSU *
  type QuantityLCSU t = DefaultLCSU

  -- | The numerical representation associated with @t@. Defaults to 'Double'.
  type QuantityRep t :: *
  type QuantityRep t = Double

  fromQuantity :: QuantityQu t -> t
  toQuantity :: t -> QuantityQu t

type QuantityQu t = MkQu_ULN (QuantityUnit t) (QuantityLCSU t) (QuantityRep t)

instance ValidDL d l =>
         Quantity (Qu d l n) where
  type QuantityUnit (Qu d l n) = UnitOfDimFactors d l
  type QuantityLCSU (Qu d l n) = l
  type QuantityRep  (Qu d l n) = n

  fromQuantity = id
  toQuantity = id
