{-# LANGUAGE DataKinds, TypeOperators #-}

module Test.CGS where

import Data.Dimensions
import Data.Dimensions.SI
import qualified Data.Dimensions.SI.Dims as D

type CGS = MkLCSU '[ (D.Length, Centi :@ Meter)
                   , (D.Mass, Gram)
                   , (D.Time, Second) ]

type CGSLength = MkDim D.Length CGS
type CGSMass = MkDim D.Mass CGS
type CGSTime = MkDim D.Time CGS