{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, UndecidableInstances #-}
module Data.Quantity.Unit where

import Data.Singletons.Eq

-- kind annotation for base units
data UnitK star = Unit star

type family EqUnit a b where
  EqUnit (Unit a) (Unit b) = a == b

type instance a == b = EqUnit a b