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
import Data.Metrology.Combinators
import Data.Metrology.LCSU
import Data.Metrology.Units
import Test.CGS

type Position = Length

cur_pos :: MkGenQu Position lcsu Double
        -> MkGenQu Velocity lcsu Double
        -> MkGenQu Acceleration lcsu Double
        -> MkGenQu Time lcsu Double
        -> MkGenQu Position lcsu Double
cur_pos x0 v a t = x0 .+ (v .* t) .+ (0.5 *. a .* (t .^ pTwo))

siPos :: MkGenQu Position SI Double
siPos = 3 % Meter

siVel :: MkGenQu Velocity SI Double
siVel = 2 % (Meter :/ Second)

siAcc :: MkGenQu Acceleration SI Double
siAcc = 10 % (Meter :/ Second :/ Second)

siTime :: MkGenQu Time SI Double
siTime = 4 % Second

siMass :: MkGenQu Mass SI Double
siMass = 1 % (Kilo :@ Gram)

cgsPos :: MkGenQu Position CGS Double
cgsPos = 3 % Meter

cgsVel :: MkGenQu Velocity CGS Double
cgsVel = 2 % (Meter :/ Second)

cgsAcc :: MkGenQu Acceleration CGS Double
cgsAcc = 10 % (Meter :/ Second :/ Second)

cgsTime :: MkGenQu Time CGS Double
cgsTime = 4 % Second

cgsMass :: MkGenQu Mass CGS Double
cgsMass = 1 % (Kilo :@ Gram)

kinetic_energy :: MkGenQu Mass lcsu Double
               -> MkGenQu Velocity lcsu Double
               -> MkGenQu Energy lcsu Double
kinetic_energy m v = redim $ 0.5 *. m .* v .* v

momentum :: MkGenQu Mass l Double
         -> MkGenQu Velocity l Double
         -> MkGenQu Momentum l Double
momentum m v = redim $ m .* v

g_earth :: Compatible lcsu (Meter :/ (Second :^ Two))
        => MkGenQu Acceleration lcsu Double
g_earth = 9.8 % (Meter :/ (Second :^ pTwo))

distance :: MkGenQu Velocity lcsu Double
         -> MkGenQu Acceleration lcsu Double
         -> MkGenQu Time lcsu Double
         -> MkGenQu Length lcsu Double
distance v a t = redim $ v .* t .+ (0.5 *. a .* t .* t)

sum :: Num n => [Qu dims l n] -> Qu dims l n
sum = foldr (.+) zero

squareAll :: Fractional n => [Qu dims l n] -> [Qu (dims @* Two) l n]
squareAll = map (.^ pTwo)
