{- Copyright (c) 2013-4 Richard Eisenberg

This file demonstrates some of `units`'s capabilities by building up a simple
physics simulator.
-}

{-# LANGUAGE TypeOperators, TypeFamilies, QuasiQuotes #-}

module Tests.Compile.Simulator where

import Data.Metrology.SI
import Data.Metrology.Show ()
import Data.Metrology.Vector

-- We never want to add positions! QPoint protects us from this.
type Position = QPoint Length

-- +x = right
-- +y = up

-- We still want the "outer" type to be Qu, not the pair. So push the pairing
-- operation down to the Qu's representation.
type family Vec2D x where
  Vec2D (Qu d l n) = Qu d l (n, n)

-- An object in our little simulation
data Object = Object { mass :: Mass
                     , rad  :: Length
                     , pos  :: Vec2D Position
                     , vel  :: Vec2D Velocity }
  deriving Show

type Universe = [Object]

-- updating takes two passes: move everything according to their own positions
-- and gravity (ignoring pulls between objects), and then look for collisions
-- and update collided objects' positions and velocities accordingly. This might
-- fail if three objects were to collide in a row all at once.

g :: Vec2D Acceleration
g = (0,-9.8)% [si| m/s^2 |]  -- could also be Meter :/ (Second :^ sTwo)

g_universe :: Force %* Length %^ Two %/ (Mass %^ Two)
g_universe = 6.67e-11 % [si| N m^2 / kg^2 |]

update :: Time -> Universe -> Universe
update dt objs
  = let objs1 = map (updateNoColls dt objs) objs in
    updateColls objs1

-- update without taking collisions into account
updateNoColls :: Time -> Universe -> Object -> Object
updateNoColls dt univ obj@(Object { mass = m, pos = x, vel = dx })
  = let new_pos = x |.+^| dx |^*| dt  -- new position
        v1 = dx |+| g |^*| dt         -- new velocity w.r.t. downward gravity
        f = gravityAt univ x m
        a = f |^/| m
        v2 = v1 |+| a |^*| dt         -- new velocity also with mutual gravity
    in obj { pos = new_pos, vel = v2 }

-- calculate the gravity at a point from all other masses
gravityAt :: Universe -> Vec2D Position -> Mass -> Vec2D Force
gravityAt univ p m = qSum (map gravity_at_1 univ)
  where
    -- gravity caused by just one point
    gravity_at_1 (Object { mass = m1, pos = pos1 })
      = let r = p |.-.| pos1
            f = g_universe |*| m1 |*| m |*^| r |^/| (qMagnitude r |^ sThree)
        in
        if qMagnitude r |>| (zero :: Length)  -- exclude the point itself!
        then redim f 
        else zero

-- update by collisions
updateColls :: Universe -> Universe
updateColls objs
  = let collisions = findCollisions objs in
    map resolveCollision collisions

-- returns a list of collisions, as pairs of an object with, perhaps,
-- a collision partner
findCollisions :: Universe -> [(Object, Maybe Object)]
findCollisions objs
  = map (findCollision objs) objs

-- check for collisions for one particular Object
findCollision :: Universe -> Object -> (Object, Maybe Object)
findCollision [] obj = (obj, Nothing)
findCollision (other : rest) obj
  | colliding other obj
  = (obj, Just other)
  | otherwise
  = findCollision rest obj

-- are two objects in contact?
colliding :: Object -> Object -> Bool
colliding (Object { pos = x1, rad = rad1 })
          (Object { pos = x2, rad = rad2 })
  = let distance = qDistance x1 x2 in
    distance |>| (zero :: Length) && distance |<=| (rad1 |+| rad2)

-- resolve the collision between two objects, updating only the first
-- object in the pair. The second object will be updated in a separate
-- (symmetric) call.
resolveCollision :: (Object, Maybe Object) -> Object
resolveCollision (obj, Nothing) = obj
resolveCollision (obj@Object { mass = m1, rad = rad1
                             , pos = z1, vel = v1 },
                Just (Object { mass = m2, rad = rad2
                             , pos = z2, vel = v2 }))
  = let -- c :: Vec2D Length
        c = z2 |.-.| z1   -- vector from z1 to z2

        -- vc1, vc2, vd1, vc1', v1' :: Vec2D Velocity
        vc1 = c `qProject` v1  -- component of v1 along c
        vc2 = c `qProject` v2  -- component of v2 along c
        vd1 = v1 |-| vc1       -- component of v1 orthogonal to c
        vc1' = (m1 |*^| vc1 |-| m2 |*^| vc2 |+| 2 *| m2 |*^| vc2) |^/| (m1 |+| m2)
                               -- new component of v1 along c
        v1' = vc1' |+| vd1     -- new v1

          -- also, move object 1 to be out of contact with object 2
        z1' = z2 |.-^| (rad1 |+| rad2) |*^| qNormalized c
    in
    obj { pos = z1', vel = v1' }

