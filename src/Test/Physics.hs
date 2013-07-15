{-# LANGUAGE TypeOperators, ConstraintKinds, ScopedTypeVariables #-}

module Physics where

import Data.Dimensions
import SI

type Energy = Mass %* Acceleration %* Length
type Momentum = Mass %* Velocity
type Position = Length

cur_pos :: Position -> Velocity -> Acceleration -> Time -> Position
cur_pos x0 v a t = x0 .+ (v .* t) .+ (0.5 *. a .* (t .^ pTwo))

kinetic_energy :: Mass -> Velocity -> Energy
kinetic_energy m v = dim $ 0.5 *. m .* v .* v

momentum :: Mass -> Velocity -> Momentum
momentum m v = dim $ m .* v

g_earth :: Acceleration
g_earth = dim $ 9.8 % (Meter :/ (Second :^ pTwo))

distance :: Velocity -> Acceleration -> Time -> Length
distance v a t = dim $ v .* t .+ (0.5 *. a .* t .* t)

-- doesn't work, with any type and any insertions of "dim" (the dimension-safe cast
-- operator) that I've come up with:
-- sum = foldr (.+) zero

sumLength :: [Length] -> Length
sumLength = foldr (.+) (dim zero)