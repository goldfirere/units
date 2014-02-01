{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, UndecidableInstances #-}
module Data.Quantity.Dimension where

import Data.Singletons.Eq

-- kind annotation for base dimensions
data BaseDimK star = BaseDim star

type family EqBaseDim a b where
  EqBaseDim (BaseDim a) (BaseDim b) = a == b

type instance a == b = EqBaseDim a b
