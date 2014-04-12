{-# LANGUAGE DataKinds, TypeOperators #-}

module Test.CGS where

import Data.Metrology
import Data.Metrology.SI
import qualified Data.Metrology.SI.Dims as D

type CGS = MkLCSU '[ (D.Length, Centi :@ Meter)
                   , (D.Mass, Gram)
                   , (D.Time, Second) ]

type CGSLength = MkGenQu D.Length CGS Double
type CGSMass = MkGenQu D.Mass CGS Double
type CGSTime = MkGenQu D.Time CGS Double
