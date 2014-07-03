{- Tests evalType
   Copyright (c) 2014 Richard Eisenberg
-}

{-# LANGUAGE TemplateHaskell, FlexibleInstances, DataKinds #-}

module Tests.Compile.EvalType where

import Data.Metrology.TH
import Data.Metrology.SI
import Data.Metrology

instance Show $(evalType [t| Length |]) where
  show x = x `showIn` Meter

