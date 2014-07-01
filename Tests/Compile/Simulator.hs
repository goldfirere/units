{-# LANGUAGE TypeOperators, TypeFamilies #-}

module Tests.Compile.Simulator where

import Prelude hiding (sum)

import Data.Metrology
import Data.Metrology.SI.Mono
import Data.Metrology.Show ()
import Data.Metrology.Vector

type Position = QPoint Length

-- +x = right
-- +y = up

type family Vec2D x where
  Vec2D (Qu d l n) = Qu d l (n, n)

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
g = (0,-9.8)%(Meter :/ (Second :^ pTwo))

g_universe :: Force %* Length %^ Two %/ (Mass %^ Two)
g_universe = 6.67e-11 % (Newton :* Meter :^ pTwo :/ (Kilo :@ Gram :^ pTwo))

update :: Time -> Universe -> Universe
update dt objs
  = let objs1 = map (updateNoColls dt objs) objs in
    updateColls objs1

updateNoColls :: Time -> Universe -> Object -> Object
updateNoColls dt univ obj@(Object { mass = m, pos = x, vel = dx })
  = let new_pos = x |.+^| dx |^*| dt
        v1 = dx |+| g |^*| dt
        f = gravityAt univ x m
        a = f |^/| m
        v2 = v1 |+| a |^*| dt
    in obj { pos = new_pos, vel = v2 }

gravityAt :: Universe -> Vec2D Position -> Mass -> Vec2D Force
gravityAt univ p m = qSum (map gravity_at_1 univ)
  where
    gravity_at_1 (Object { mass = m1, pos = pos1 })
      = let r = p |.-.| pos1
            f = g_universe |*| m1 |*| m |*^| r |^/| (qMagnitude r |^ pThree)
        in
        if qMagnitude r |>| (zero :: Length)
        then redim f 
        else zero


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
colliding (Object { pos = x1, rad = rad1 })
          (Object { pos = x2, rad = rad2 })
  = let distance = qDistance x1 x2 in
    distance |>| (zero :: Length) && distance |<=| (rad1 |+| rad2)

resolveCollision :: (Object, Maybe Object) -> Object
resolveCollision (obj, Nothing) = obj
resolveCollision (obj@Object { mass = m1, rad = rad1
                             , pos = z1, vel = v1 },
                Just (Object { mass = m2, rad = rad2
                             , pos = z2, vel = v2 }))
  = let -- c :: Vec2D Length
        c = z2 |.-.| z1

        -- vc1, vc2, vd1, vc1', v1' :: Vec2D Velocity
        vc1 = c `qProject` v1
        vc2 = c `qProject` v2
        vd1 = v1 |-| vc1
        vc1' = (m1 |*^| vc1 |-| m2 |*^| vc2 |+| 2 *| m2 |*^| vc2) |^/| (m1 |+| m2)
        v1' = vc1' |+| vd1

        z1' = z2 |.-^| (rad1 |+| rad2) |*^| qNormalized c
    in
    obj { pos = z1', vel = v1' }

