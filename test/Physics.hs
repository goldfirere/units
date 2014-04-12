{-# LANGUAGE TypeOperators, ConstraintKinds, ScopedTypeVariables, TypeFamilies,
             FlexibleContexts, DataKinds #-}

module Test.Physics where

import Data.Metrology
import Data.Metrology.Poly
import Data.Metrology.SI.Dims
import Data.Metrology.SI.Units
import Data.Metrology.SI ( SI )
import Data.Metrology.Show
import Data.Metrology.SI.Prefixes
import Data.Metrology.UnitCombinators
import Data.Metrology.LCSU
import Data.Metrology.Units
import Test.CGS

type Position = Length

cur_pos :: MkGenDim Position lcsu Double
        -> MkGenDim Velocity lcsu Double
        -> MkGenDim Acceleration lcsu Double
        -> MkGenDim Time lcsu Double
        -> MkGenDim Position lcsu Double
cur_pos x0 v a t = x0 .+ (v .* t) .+ (0.5 *. a .* (t .^ pTwo))

siPos :: MkGenDim Position SI Double
siPos = 3 % Meter

siVel :: MkGenDim Velocity SI Double
siVel = 2 % (Meter :/ Second)

siAcc :: MkGenDim Acceleration SI Double
siAcc = 10 % (Meter :/ Second :/ Second)

siTime :: MkGenDim Time SI Double
siTime = 4 % Second

siMass :: MkGenDim Mass SI Double
siMass = 1 % (Kilo :@ Gram)

cgsPos :: MkGenDim Position CGS Double
cgsPos = 3 % Meter

cgsVel :: MkGenDim Velocity CGS Double
cgsVel = 2 % (Meter :/ Second)

cgsAcc :: MkGenDim Acceleration CGS Double
cgsAcc = 10 % (Meter :/ Second :/ Second)

cgsTime :: MkGenDim Time CGS Double
cgsTime = 4 % Second

cgsMass :: MkGenDim Mass CGS Double
cgsMass = 1 % (Kilo :@ Gram)

kinetic_energy :: MkGenDim Mass lcsu Double
               -> MkGenDim Velocity lcsu Double
               -> MkGenDim Energy lcsu Double
kinetic_energy m v = dim $ 0.5 *. m .* v .* v

momentum :: MkGenDim Mass l Double
         -> MkGenDim Velocity l Double
         -> MkGenDim Momentum l Double
momentum m v = dim $ m .* v

g_earth :: Compatible lcsu (Meter :/ (Second :^ Two))
        => MkGenDim Acceleration lcsu Double
g_earth = 9.8 % (Meter :/ (Second :^ pTwo))

distance :: MkGenDim Velocity lcsu Double
         -> MkGenDim Acceleration lcsu Double
         -> MkGenDim Time lcsu Double
         -> MkGenDim Length lcsu Double
distance v a t = dim $ v .* t .+ (0.5 *. a .* t .* t)

sum :: Num n => [Dim dims l n] -> Dim dims l n
sum = foldr (.+) zero

squareAll :: Fractional n => [Dim dims l n] -> [Dim (dims @* Two) l n]
squareAll = map (.^ pTwo)
