-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.SI
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports unit, type, and prefix definitions according to the SI
-- system of units. The definitions were taken from here:
-- <http://www.bipm.org/en/si/> and here:
-- <http://www.bipm.org/utils/common/documents/jcgm/JCGM_200_2012.pdf>.
--
-- This module exports the monomorphic version of the definitions. For
-- polymorphic versions, use 'Data.Metrology.SI.Poly'.
-----------------------------------------------------------------------------

module Data.Metrology.SI (
  module Data.Metrology.SI.Mono,
  ) where

import Data.Metrology.SI.Mono
