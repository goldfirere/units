{-# LANGUAGE DataKinds, TypeOperators #-}

module Test.CGS where

import Data.Metrology
import Data.Metrology.SI
import qualified Data.Metrology.SI.Dims as D

type CGS = MkLCSU '[ (D.Length, Centi :@ Meter)
                   , (D.Mass, Gram)
                   , (D.Time, Second) ]

type CGSLength = MkQu_DLN D.Length CGS Double
type CGSMass = MkQu_DLN D.Mass CGS Double
type CGSTime = MkQu_DLN D.Time CGS Double
