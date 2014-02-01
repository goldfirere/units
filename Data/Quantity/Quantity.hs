{-# LANGUAGE DataKinds, FlexibleInstances, FlexibleContexts, KindSignatures, MultiParamTypeClasses, TypeFamilies #-}
module Data.Quantity.Quantity where

import Data.Quantity.Dimension
import Data.Quantity.Unit
import Data.Quantity.Zahl

newtype Qu (lcsu :: [(BaseDimK *, BaseUnitK *)]) (unit :: [(BaseUnitK *, Zahl)])  (value :: *) = Qu value
  deriving (Eq, Show)
