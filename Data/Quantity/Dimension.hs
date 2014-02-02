{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, UndecidableInstances #-}
module Data.Quantity.Dimension where

import Data.Singletons.Eq

-- kind annotation for base dimensions
data DimK star = Dim star

type family EqDim a b where
  EqDim (Dim a) (Dim b) = a == b

type instance a == b = EqDim a b
