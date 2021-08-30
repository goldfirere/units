{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.Eurocard
-- Copyright   :  (C) 2018 Mario Lang
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The units of length as defined by the eurocard standard.
--
-- See <https://en.wikipedia.org/wiki/Eurocard_(printed_circuit_board)> or IEC 60297.
-----------------------------------------------------------------------------

module Data.Units.Eurocard (
  -- * Lengths
  RackUnit(..), HorizontalPitch(..)
  ) where

import Data.Metrology.TH
import Data.Units.US (Inch)

declareDerivedUnit "HorizontalPitch" [t| Inch |] 0.2  (Just "HP")
declareDerivedUnit "RackUnit"        [t| Inch |] 1.75 (Just "U")
