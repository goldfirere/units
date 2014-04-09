{-# LANGUAGE DataKinds, TypeOperators #-}

module Test.CGS where

import Data.Dimensions
import Data.Dimensions.SI
import qualified Data.Dimensions.SI.Dims as D

type CGS = MkLCSU '[ (D.Length, Centi :@ Meter)
                   , (D.Mass, Gram)
                   , (D.Time, Second) ]

type CGSLength = MkGenDim D.Length CGS Double
type CGSMass = MkGenDim D.Mass CGS Double
type CGSTime = MkGenDim D.Time CGS Double
