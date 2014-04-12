{-# LANGUAGE TypeFamilies, TypeOperators #-}

module Test.Readme where

    import Data.Metrology

    data Meter = Meter    -- each unit is a datatype that acts as its own proxy
    instance Unit Meter where           -- declare Meter as a Unit
      type BaseUnit Meter = Canonical   -- Meters are "canonical"
    instance Show Meter where           -- Show instances are optional but useful
      show _ = "m"                      -- do *not* examine the argument!

    data Foot = Foot
    instance Unit Foot where
      type BaseUnit Foot = Meter        -- Foot is defined in terms of Meter
      conversionRatio _ = 0.3048        -- do *not* examine the argument!

    type Length = MkDim Meter           -- we will manipulate Lengths
    type Length' = MkDim Foot           -- this is the *same* as Length

    extend :: Length -> Length          -- a function over lengths
    extend x = dim $ x .+ (1 % Meter)   -- more on this later

    inMeters :: Length -> Double        -- extract the # of meters
    inMeters = (# Meter)                -- more on this later


    data Kilo = Kilo
    instance UnitPrefix Kilo where
      multiplier _ = 1000

    kilo :: unit -> Kilo :@ unit
    kilo = (Kilo :@)

    longWayAway :: Length
    longWayAway = 150 % kilo Meter

    longWayAwayInMeters :: Double
    longWayAwayInMeters = longWayAway # Meter  -- 150000.0


    data Second = Second
    instance Unit Second where
      type BaseUnit Second = Canonical
    instance Show Second where
      show _ = "s"

    type Time = MkDim Second


    type MetersPerSecond = Meter :/ Second
    type Velocity1 = MkDim MetersPerSecond

    speed :: Velocity1
    speed = 20 % (Meter :/ Second)

    type Velocity2 = Length %/ Time    -- same type as Velocity1

    type MetersSquared = Meter :^ Two
    type Area1 = MkDim MetersSquared
    type Area2 = Length %^ Two        -- same type as Area1

    roomSize :: Area1
    roomSize = 100 % (Meter :^ pTwo)

    roomSize' :: Area1
    roomSize' = 100 % (Meter :* Meter)

    type Velocity3 = Scalar %/ Time %* Length
    addVels :: Velocity1 -> Velocity1 -> Velocity3
    addVels v1 v2 = dim $ v1 .+ v2
