{- Data/Metrology/Units.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   rae@cs.brynmawr.edu

   This file defines the class Dimension, which is needed for
   defining dimensions.
-}

{-# LANGUAGE TypeFamilies, DataKinds, DefaultSignatures, MultiParamTypeClasses,
             ConstraintKinds, UndecidableInstances, FlexibleContexts,
             FlexibleInstances, ScopedTypeVariables, TypeOperators, CPP #-}

#if __GLASGOW_HASKELL__ >= 900
{-# OPTIONS_GHC -Wno-star-is-type #-}
#endif

module Data.Metrology.Dimensions where

import Data.Metrology.Z
import Data.Metrology.Factor

-- | This class is used to mark abstract dimensions, such as @Length@, or
-- @Mass@.
class Dimension dim where
  -- | Retrieve a list of @Factor@s representing the given dimension. Overriding
  -- the default of this type family should not be necessary in user code.
  type DimFactorsOf dim :: [Factor *]
  type DimFactorsOf dim = '[F dim One]
