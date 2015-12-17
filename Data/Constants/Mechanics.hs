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

-- | Gravitational constant (value recommended by CODATA, see the
-- <http://arxiv.org/abs/1507.07956 arXiv> paper)
declareConstant "gravity_G" 6.6740831e-11
  [t| (Meter :^ Three) :* ((Kilo :@ Gram) :^ MOne) :* (Second :^ MTwo) |]
  -- Thanks to @hesiod, PR #13
