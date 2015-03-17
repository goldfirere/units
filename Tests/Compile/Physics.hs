{-# LANGUAGE TypeOperators, ConstraintKinds, ScopedTypeVariables, TypeFamilies,
             FlexibleContexts, DataKinds #-}

module Tests.Compile.Physics where

import Data.Dimensions.SI
import Data.Metrology.Poly
import Data.Metrology.SI.Poly ( SI )
import Data.Units.SI
import Data.Units.SI.Prefixes
import Tests.Compile.CGS

type Position = Length

cur_pos :: MkQu_DLN Position lcsu Double
        -> MkQu_DLN Velocity lcsu Double
        -> MkQu_DLN Acceleration lcsu Double
        -> MkQu_DLN Time lcsu Double
        -> MkQu_DLN Position lcsu Double
cur_pos x0 v a t = x0 |+| (v |*| t) |+| (0.5 *| a |*| (t |^ sTwo))

siPos :: MkQu_DLN Position SI Double
siPos = 3 % Meter

siVel :: MkQu_DLN Velocity SI Double
siVel = 2 % (Meter :/ Second)

siAcc :: MkQu_DLN Acceleration SI Double
siAcc = 10 % (Meter :/ Second :/ Second)

siTime :: MkQu_DLN Time SI Double
siTime = 4 % Second

siMass :: MkQu_DLN Mass SI Double
siMass = 1 % (Kilo :@ Gram)

cgsPos :: MkQu_DLN Position CGS Double
cgsPos = 3 % Meter

cgsVel :: MkQu_DLN Velocity CGS Double
cgsVel = 2 % (Meter :/ Second)

cgsAcc :: MkQu_DLN Acceleration CGS Double
cgsAcc = 10 % (Meter :/ Second :/ Second)

cgsTime :: MkQu_DLN Time CGS Double
cgsTime = 4 % Second

cgsMass :: MkQu_DLN Mass CGS Double
cgsMass = 1 % (Kilo :@ Gram)

kinetic_energy :: MkQu_DLN Mass lcsu Double
               -> MkQu_DLN Velocity lcsu Double
               -> MkQu_DLN Energy lcsu Double
kinetic_energy m v = redim $ 0.5 *| m |*| v |*| v

momentum :: MkQu_DLN Mass l Double
         -> MkQu_DLN Velocity l Double
         -> MkQu_DLN Momentum l Double
momentum m v = redim $ m |*| v

g_earth :: CompatibleUnit lcsu (Meter :/ (Second :^ Two))
        => MkQu_DLN Acceleration lcsu Double
g_earth = 9.8 % (Meter :/ (Second :^ sTwo))

distance :: MkQu_DLN Velocity lcsu Double
         -> MkQu_DLN Acceleration lcsu Double
         -> MkQu_DLN Time lcsu Double
         -> MkQu_DLN Length lcsu Double
distance v a t = redim $ v |*| t |+| (0.5 *| a |*| t |*| t)

sum :: Num n => [Qu dims l n] -> Qu dims l n
sum = foldr (|+|) zero

squareAll :: Fractional n => [Qu dims l n] -> [Qu (dims @* Two) l n]
squareAll = map (|^ sTwo)
