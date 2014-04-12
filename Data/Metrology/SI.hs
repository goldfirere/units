{-# LANGUAGE TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.SI
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports unit, type, and prefix definitions according to the SI
-- system of units. The definitions were taken from here:
-- <http://www.bipm.org/en/si/> and here:
-- <http://www.bipm.org/utils/common/documents/jcgm/JCGM_200_2012.pdf>.
--
-- There is one deviation from the definition at that site: To work better
-- with prefixes, the unit of mass is 'Gram'.
-----------------------------------------------------------------------------

module Data.Metrology.SI (
  module Data.Metrology.SI.Units,
  module Data.Metrology.SI.Types,
  module Data.Metrology.SI.Prefixes
  ) where

import Data.Metrology.SI.Units
import Data.Metrology.SI.Types
import Data.Metrology.SI.Prefixes

