{-# LANGUAGE TypeOperators, ConstraintKinds, TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Constants.Mechanics
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This file defines dimensioned physical constants, useful in mechanics.
--
-- The names used are a short description of the constant followed by its
-- usual symbol, separated by an underscore. For non-Latin symbols, the
-- Latin-lettered transliteration of the symbol name is used.
-----------------------------------------------------------------------------

module Data.Constants.Mechanics where

import Data.Metrology.Poly
import Data.Metrology.SI.Poly
import Data.Metrology.TH

-- | Acceleration at Earth's surface due to gravity.
declareConstant "gravity_g" 9.80665 [t| Meter :/ Second :^ Two |]
