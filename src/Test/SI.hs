{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}

module SI where

import Data.Dimensions

------ Basic units -------------

data Meter = Meter
type Length = MkDim Meter

instance Unit Meter where
  type BaseUnit Meter = Canonical
instance Show Meter where
  show _ = "m"

data Second = Second
type Time = MkDim Second

instance Unit Second where
  type BaseUnit Second = Canonical
instance Show Second where
  show _ = "s"

data Kg = Kg
type Mass = MkDim Kg

instance Unit Kg where
  type BaseUnit Kg = Canonical
instance Show Kg where
  show _ = "kg"

------- Derived units --------

type Velocity = Length %/ Time
type Acceleration = Length %/ (Time %^ Two)

------- SI Prefixes ----------

data KiloPrefix = KiloPrefix
instance UnitPrefix KiloPrefix where
  multiplier _ = 1000

type Kilo a = KiloPrefix :@ a
kilo :: unit -> Kilo unit
kilo = (KiloPrefix :@)
