{-# LANGUAGE DataKinds #-}
module Data.Quantity.Dimension where

-- kind annotation for base dimensions
data BaseDimK star = BaseDim star
