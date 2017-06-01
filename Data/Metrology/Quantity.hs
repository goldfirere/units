-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Quantity
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports a class 'Quantity' to allow easy conversion between proper
-- quantities and types from other libraries.
------------------------------------------------------------------------------

{-# LANGUAGE DataKinds, TypeFamilies, ConstraintKinds, UndecidableInstances #-}

module Data.Metrology.Quantity where

import Data.Metrology.Poly

-- | 'Quantity' allows for easy conversions in and out of quantities. For example,
-- say you are working with an outside library for time that defines `UTCTime`, where
-- that stores the time measured in seconds. You could say
--
-- > instance Quantity UTCTime where
-- >   type QuantityUnit = Second
-- >   fromQuantity = ...
-- >   toQuantity = ...
--
-- Then, conversions are easy and unit-safe.
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

-- | The 'Qu' type associated with a member of the 'Quantity' class
type QuantityQu t = MkQu_ULN (QuantityUnit t) (QuantityLCSU t) (QuantityRep t)

instance ValidDL d l =>
         Quantity (Qu d l n) where
  type QuantityUnit (Qu d l n) = UnitOfDimFactors d l
  type QuantityLCSU (Qu d l n) = l
  type QuantityRep  (Qu d l n) = n

  fromQuantity = id
  toQuantity = id
