{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.SI.Parser
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines a quasi-quoting parser for unit expressions. Writing, say,
-- @[si| m/s^2 |]@ produces @(Meter :/ (Second :^ sTwo))@. A larger
-- example is
--
-- > ke :: Energy
-- > ke = 5 % [si| N km |]  -- 5 Newton-kilometers
--
-- See `Data.Metrology.Parser` for more information about the syntax
-- of these unit expressions.
-----------------------------------------------------------------------------

module Data.Units.SI.Parser ( si ) where

import Data.Metrology.Parser
import Data.Units.SI.Prefixes
import Data.Units.SI

makeQuasiQuoter "si" siPrefixes siUnits
