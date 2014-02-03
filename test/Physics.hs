{-# LANGUAGE TypeOperators, ConstraintKinds, ScopedTypeVariables #-}

module Test.Physics where

import Data.Dimensions
import Data.Dimensions.Poly
import Data.Dimensions.SI
import Data.Dimensions.SI.Types

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

sum :: Num n => [Dim n dims] -> Dim n dims
sum = foldr (.+) zero

squareAll :: Fractional n => [Dim n dims] -> [Dim n (dims @* Two)]
squareAll = map (.^ pTwo)