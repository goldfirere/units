{- Data/Dimensions/Internal.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu
-}

{-# LANGUAGE ExplicitNamespaces #-}

{-| This module gathers and exports all parts of the units package that might
    be useful, even when going past the abstraction layer of the package.

    With the exports from this module, it is possible to perform unsafe
    operations that do not respect the rules of dimensional analysis. Use with
    caution.

    Additionally, no attempt will be made to keep the exports of this module
    backward compatible.
-}

module Data.Dimensions.Internal (
  -- * The @Dim@ type
  Dim(..),

  -- * Manipulating dimension specifications
  DimSpec(..), type ($=), Extract, Reorder, type (@~), Normalize,

  type (@+), type (@-), NegDim, NegList, type (@*), type (@/),

  ) where

import Data.Dimensions.Dim
import Data.Dimensions.DimSpec
