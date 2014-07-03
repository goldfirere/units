-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.TH
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports Template Haskell functions to make working with
-- @units@ a little more convenient.
-----------------------------------------------------------------------------

module Data.Metrology.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Desugar.Expand

-- | "Evaluates" a type as far as it can. This is useful, say, in instance
-- declarations:
--
-- > instance Show $(evalType [t| Length |]) where ...
--
-- Without the 'evalType', the instance declaration fails because @Length@
-- mentions type families, which can't be used in instance declarations.
--
-- This function is somewhat experimental, and will likely not work with
-- more polymorphic types. (If it doesn't work, not all of the type families
-- will be evaluated, and the instance declaration will fail. This function
-- should never cause /incorrect/ behavior.)
evalType :: Q Type -> Q Type
evalType qty = do
  ty <- qty
  dty <- dsType ty
  ex_dty <- expandType dty
  return $ sweeten ex_dty
