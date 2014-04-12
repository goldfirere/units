{- Data/Metrology/Units.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file defines the class Unit, which is needed for
   user-defined units.
-}

{-# LANGUAGE TypeFamilies, DataKinds, DefaultSignatures, MultiParamTypeClasses,
             ConstraintKinds, UndecidableInstances, FlexibleContexts,
             FlexibleInstances, ScopedTypeVariables, TypeOperators #-}

module Data.Metrology.Dimensions where

import Data.Metrology.Z
import Data.Metrology.DimSpec
import Data.Metrology.LCSU
import Data.Type.Bool
import Data.Type.Equality
import Data.Proxy
import Data.Singletons
import GHC.Exts

-- | Dummy type use just to label canonical units. It does /not/ have a
-- 'Unit' instance.
data Canonical

-- | This class is used to mark abstract dimensions, such as @Length@, or
-- @Mass@.
class Dimension dim where
  -- | Retrieve a list of @DimSpec@s representing the given dimension. Overriding
  -- the default of this type family should not be necessary in user code.
  type DimSpecsOf dim :: [DimSpec *]
  type DimSpecsOf dim = '[D dim One]
  
