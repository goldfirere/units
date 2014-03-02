{-# LANGUAGE TypeOperators, TypeFamilies #-}

module Test.Simulator where

import Data.Dimensions
import Data.Dimensions.SI
import Data.Dimensions.SI.Types
import Data.Dimensions.SI.Prefixes
import Data.Dimensions.Show
import Data.Dimensions.Poly

type Position = Length

-- +x = right
-- +y = up

data Object = Object { mass :: Mass
                     , rad  :: Length
                     , pos  :: (Position, Position)
                     , vel  :: (Velocity, Velocity) }
  deriving Show

type Universe = [Object]

-- updating takes two passes: move everything according to their own positions
-- and gravity (ignoring pulls between objects), and then look for collisions
-- and update collided objects' positions and velocities accordingly. This might
-- fail if three objects were to collide in a row all at once.

g :: Acceleration
g = (-9.8)%(Meter :/ (Second :^ pTwo))

g_universe :: Force %* Length %^ Two %/ (Mass %^ Two)
g_universe = 6.67e-11 % (Newton :* Meter :^ pTwo :/ (Kilo :@ Gram :^ pTwo))

update :: Time -> Universe -> Universe
update dt objs
  = let objs1 = map (updateNoColls dt objs) objs in
    updateColls objs1

updateNoColls :: Time -> Universe -> Object -> Object
updateNoColls dt univ obj@(Object { mass = m, pos = (x, y), vel = (dx, dy) })
  = let new_pos = (x .+ dx .* dt, y .+ dy .* dt)
        v1 = (dx, dy .+ g .* dt)
        f = gravityAt univ (x, y) m
        a = f !/ m
        v2 = v1 !+ dt !* a
    in obj { pos = new_pos, vel = v2 }

gravityAt :: Universe -> (Position, Position) -> Mass -> (Force, Force)
gravityAt univ pos m = sum (map gravity_at_1 univ)
  where
    gravity_at_1 (Object { mass = m1, pos = pos1 })
      = let r = mag (pos1 !- pos)
            f = g_universe .* m1 .* m ./ r .^ pTwo
        in
        if r .> (zero :: Length)
        then dimPair $ f !* ((pos1 !- pos) !/ r)
        else (zero, zero)

    sum :: [(Force, Force)] -> (Force, Force)
    sum [] = (zero, zero)
    sum (h : t) = h !+ sum t

    dimPair (a,b) = (dim a, dim b)

updateColls :: Universe -> Universe
updateColls objs
  = let collisions = findCollisions objs in
    map resolveCollision collisions

findCollisions :: Universe -> [(Object, Maybe Object)]
findCollisions objs
  = map (findCollision objs) objs

findCollision :: Universe -> Object -> (Object, Maybe Object)
findCollision [] obj = (obj, Nothing)
findCollision (other : rest) obj
  | colliding other obj
  = (obj, Just other)
  | otherwise
  = findCollision rest obj

colliding :: Object -> Object -> Bool
colliding (Object { pos = (x1, y1), rad = rad1 })
          (Object { pos = (x2, y2), rad = rad2 })
  = let distance = dimSqrt $ (x1 .- x2) .^ pTwo .+ (y1 .- y2) .^ pTwo in
    distance .> (zero :: Length) && distance .<= (rad1 .+ rad2)

infixl 7 `dot`
dot (a,b) (c,d) = a .* c .+ b .* d

infixl 6 !+
(a,b) !+ (c,d) = (a .+ c, b .+ d)

infixl 6 !-
(a,b) !- (c,d) = (a .- c, b .- d)

infixl 7 !*
a !* (c,d) = (a .* c, a .* d)

infixl 7 !/
(a,b) !/ c = (a ./ c, b ./ c)

mag (a,b) = dimSqrt $ (a .^ pTwo) .+ (b .^ pTwo)

resolveCollision :: (Object, Maybe Object) -> Object
resolveCollision (obj, Nothing) = obj
resolveCollision (obj@Object { mass = m1, rad = rad1
                             , pos = z1, vel = v1 },
                Just (Object { mass = m2, rad = rad2
                             , pos = z2, vel = v2 }))
  = let c = z2 !- z1
        c_hat = (mag c) .^ pMOne !* c
        vc1 = v1 `dot` c_hat
        vd1 = v1 !- (vc1 !* c_hat)
        vc2 = v2 `dot` c ./ mag c
        vc1' = (m1 .* vc1 .- m2 .* vc2 .+ 2 *. m2 .* vc2) ./ (m1 .+ m2)
        v1' = vc1' !* c_hat !+ vd1

        z1' = z2 !- (rad1 .+ rad2) !* c_hat
    in
    obj { pos = z1', vel = v1' }