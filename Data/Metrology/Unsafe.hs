{-# LANGUAGE Unsafe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Unsafe
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports the constructor of the 'Dim' type. This allows you
-- to write codes that takes values in and out of quantity at your will, 
-- that may lead to dimension unsafety. Use at your peril.
-----------------------------------------------------------------------------

module Data.Metrology.Unsafe (
  -- * The 'Dim' type
  Qu(..),
  ) where

import Data.Metrology.Quantity

