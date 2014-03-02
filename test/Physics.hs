{-# LANGUAGE TypeOperators, ConstraintKinds, ScopedTypeVariables, TypeFamilies,
             FlexibleContexts #-}

module Test.Physics where

import Data.Dimensions
import Data.Dimensions.Poly
import Data.Dimensions.SI.Dims
import Data.Dimensions.SI.Units
import Data.Dimensions.SI ( SI )
import Data.Dimensions.Show
import Data.Dimensions.SI.Prefixes
import Data.Dimensions.UnitCombinators
import Data.Dimensions.Map
import Data.Dimensions.Units
import Test.CGS

type Position = Length

cur_pos :: MkDim Position lcsu
        -> MkDim Velocity lcsu
        -> MkDim Acceleration lcsu
        -> MkDim Time lcsu
        -> MkDim Position lcsu
cur_pos x0 v a t = x0 .+ (v .* t) .+ (0.5 *. a .* (t .^ pTwo))

siPos :: MkDim Position SI
siPos = 3 % Meter

siVel :: MkDim Velocity SI
siVel = 2 % (Meter :/ Second)

siAcc :: MkDim Acceleration SI
siAcc = 10 % (Meter :/ Second :/ Second)

siTime :: MkDim Time SI
siTime = 4 % Second

siMass :: MkDim Mass SI
siMass = 1 % (Kilo :@ Gram)

cgsPos :: MkDim Position CGS
cgsPos = 3 % Meter

cgsVel :: MkDim Velocity CGS
cgsVel = 2 % (Meter :/ Second)

cgsAcc :: MkDim Acceleration CGS
cgsAcc = 10 % (Meter :/ Second :/ Second)

cgsTime :: MkDim Time CGS
cgsTime = 4 % Second

cgsMass :: MkDim Mass CGS
cgsMass = 1 % (Kilo :@ Gram)

kinetic_energy :: MkDim Mass lcsu
               -> MkDim Velocity lcsu
               -> MkDim Energy lcsu
kinetic_energy m v = dim $ 0.5 *. m .* v .* v

momentum :: MkDim Mass l
         -> MkDim Velocity l
         -> MkDim Momentum l
momentum m v = dim $ m .* v

g_earth :: ( Compatible Time lcsu Second
           , Compatible Length lcsu Meter )
        => MkDim Acceleration lcsu
g_earth = 9.8 % (Meter :/ (Second :^ pTwo))

distance :: MkDim Velocity lcsu
         -> MkDim Acceleration lcsu
         -> MkDim Time lcsu
         -> MkDim Length lcsu
distance v a t = dim $ v .* t .+ (0.5 *. a .* t .* t)

sum :: Num n => [Dim n dims l] -> Dim n dims l
sum = foldr (.+) zero

squareAll :: Fractional n => [Dim n dims l] -> [Dim n (dims @* Two) l]
squareAll = map (.^ pTwo)
