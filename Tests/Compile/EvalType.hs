{- Tests evalType
   Copyright (c) 2014 Richard Eisenberg
-}

{-# LANGUAGE TemplateHaskell, FlexibleInstances, DataKinds, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Compile.EvalType where

import Data.Metrology.TH
import Data.Metrology.SI
import Data.Metrology

instance Show $(evalType [t| Length |]) where
  show x = x `showIn` Meter

#if MIN_VERSION_th_desugar(1,5,0)
instance Show $(evalType [t| Volume |]) where
  show x = x `showIn` Liter
#endif
