{-# LANGUAGE TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dimensions.SI
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports unit, type, and prefix definitions according to the SI
-- system of units. The definitions were taken from here:
-- <http://www.bipm.org/en/si/>.
--
-- There is one deviation from the definition at that site: To work better
-- with prefixes, the unit of mass is 'Gram'.
-----------------------------------------------------------------------------

module Data.Dimensions.SI (
  module Data.Dimensions.SI.Units,
  module Data.Dimensions.SI.Types,
  module Data.Dimensions.SI.Prefixes
  ) where

import Data.Dimensions.SI.Units
import Data.Dimensions.SI.Types
import Data.Dimensions.SI.Prefixes

