{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds #-}

module Tests.Compile.Readme where

    import Data.Metrology.Poly hiding (LCSU)

    data LengthDim = LengthDim  -- each dimension is a datatype that acts as its own proxy
    instance Dimension LengthDim

    data TimeDim = TimeDim
    instance Dimension TimeDim

    type VelocityDim = LengthDim :/ TimeDim
    data Meter = Meter
    instance Unit Meter where           -- declare Meter as a Unit
      type BaseUnit Meter = Canonical   -- Meters are "canonical"
      type DimOfUnit Meter = LengthDim  -- Meters measure Lengths
    instance Show Meter where           -- Show instances are optional but useful
      show _ = "m"                      -- do *not* examine the argument!

    data Foot = Foot
    instance Unit Foot where
      type BaseUnit Foot = Meter        -- Foot is defined in terms of Meter
      conversionRatio _ = 0.3048        -- do *not* examine the argument!
                                        -- We don't need to specify the `DimOfUnit`;
                                        -- it's implied by the `BaseUnit`.
    instance Show Foot where
      show _ = "ft"

    data Second = Second
    instance Unit Second where
      type BaseUnit Second = Canonical
      type DimOfUnit Second = TimeDim
    instance Show Second where
      show _ = "s"

    type LCSU = MkLCSU '[(LengthDim, Meter), (TimeDim, Second)]

    type Length = MkQu_DLN LengthDim LCSU Double
      -- Length stores lengths in our defined LCSU, using `Double` as the numerical type
    type Length' = MkQu_ULN Foot LCSU Double
      -- same as Length. Note the `U` in `MkQu_ULN`, allowing it to take a unit

    type Time = MkQu_DLN TimeDim LCSU Double

    extend :: Length -> Length            -- a function over lengths
    extend x = redim $ x |+| (1 % Meter)

    inMeters :: Length -> Double          -- extract the # of meters
    inMeters = (# Meter)                  -- more on this later

    conversion :: Length                  -- mixing units
    conversion = (4 % Meter) |+| (10 % Foot)

    vel :: Length %/ Time                 -- The `%*` and `%/` operators allow
                                          -- you to combine types
    vel = (3 % Meter) |/| (2 % Second)

    data Kilo = Kilo
    instance UnitPrefix Kilo where
      multiplier _ = 1000

    kilo :: unit -> Kilo :@ unit
    kilo = (Kilo :@)

    longWayAway :: Length
    longWayAway = 150 % kilo Meter

    longWayAwayInMeters :: Double
    longWayAwayInMeters = longWayAway # Meter  -- 150000.0

    type MetersPerSecond = Meter :/ Second
    type Velocity1 = MkQu_ULN MetersPerSecond LCSU Double

    speed :: Velocity1
    speed = 20 % (Meter :/ Second)

    type Velocity2 = Length %/ Time    -- same type as Velocity1

    type MetersSquared = Meter :^ Two
    type Area1 = MkQu_ULN MetersSquared LCSU Double
    type Area2 = Length %^ Two        -- same type as Area1

    roomSize :: Area1
    roomSize = 100 % (Meter :^ sTwo)

    roomSize' :: Area1
    roomSize' = 100 % (Meter :* Meter)

    type Velocity3 = (MkQu_ULN Number LCSU Double) %/ Time %* Length
    addVels :: Velocity1 -> Velocity1 -> Velocity3
    addVels v1 v2 = redim $ (v1 |+| v2)

    type instance DefaultUnitOfDim LengthDim = Meter
    type instance DefaultUnitOfDim TimeDim   = Second

    -- type Length = MkQu_D LengthDim

