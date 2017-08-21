-- Copyright (c) 2011 - All rights reserved - Keera Studios
module Data where

import Data.IORef

-- * Physical dimensions
type Pos2D  = (Double, Double)

pointX :: Pos2D -> Double
pointX = fst

pointY :: Pos2D -> Double
pointY = snd

type Size2D = (Double, Double)

-- * Auxiliary

-- ** Vector and matrix operations

-- *** Vector 2
type Vec2 a = (a, a)

(^+^) :: Num a => Vec2 a -> Vec2 a -> Vec2 a
(x1, y1) ^+^ (x2, y2) = (x1+x2, y1+y2)

(^-^) :: Num a => Vec2 a -> Vec2 a -> Vec2 a
(x1, y1) ^-^ (x2, y2) = (x1-x2, y1-y2)

normalize :: Floating a => Vec2 a -> Vec2 a
normalize v@(vx, vy) = (vx / m, vy / m)
 where m = vectorMagnitude v

vectorMagnitude ::  Floating a => Vec2 a -> a
vectorMagnitude (vx, vy) = sqrt (vx**2 + vy**2)

vectorWithMagnitude :: Pos2D -> Double -> Pos2D
vectorWithMagnitude (0,0)   _       = (0, 0)
vectorWithMagnitude v@(x,y) desired = (x*prop, y*prop)
 where prop = desired / vectorMagnitude v

rotateRespect (vx, vy) = rotateRespect' (normalize (vx, -vy))
unrotateRespect (vx, vy) = rotateRespect' (normalize (vx, vy))
rotateRespect' (ct, st) (px, py) = (ct * px - st * py, st * px + ct * py)

-- *** Vector 3
type Vec3 a = (a, a, a)

(^*^) :: Num a => Vec3 a -> Vec3 a -> Vec3 a
(x1,x2,x3) ^*^ (y1, y2, y3) = (x1*y1, x2*y2, x3*y3)

sumV :: Num a => Vec3 a -> a
sumV (x1,x2,x3) = x1 + x2 + x3

-- *** Matrix 3
type Mat3 a = (Vec3 a, Vec3 a, Vec3 a)

-- (^*.) :: Mat3 a -> Vec3 a -> Vec3 a
-- (r1, r2, r3) :*: v =
--   ( sumV (r1 ^*^ v)
--   , sumV (r2 ^*^ v)
--   , sumV (r3 ^*^ v)
--   )

minimumWith :: Ord b => (a -> b) -> [a] -> a
minimumWith f (a:as) = minimumWith' f a as

minimumWith' :: Ord b => (a -> b) -> a -> [a] -> a
minimumWith' f mn [] = mn
minimumWith' f mn (a:as) = minimumWith' f mn' as
 where mn' = if f mn < f a then mn else a

swap (a,b) = (b,a)
