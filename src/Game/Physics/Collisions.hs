{-# LANGUAGE FlexibleContexts       #-}
-- | A trivial collision subsystem.
--
-- Based on the physics module, it determines the side of collision
-- between shapes.
module Game.Physics.Collisions where

import Data.Extra.Num
import Data.Maybe
import FRP.Yampa.VectorSpace
import Physics.TwoDimensions.Dimensions
import Physics.TwoDimensions.PhysicalObjects
import Physics.Collisions

import Game.Physics.Shapes

-- * Collision points
data CollisionPoint = CollisionSide  Side
                    | CollisionAngle Double

-- | Calculates the collision side of a shape
-- that collides against another.
--
-- PRE: the shapes do collide. Use 'overlapShape' to check.
shapeCollisionPoint :: Shape -> Shape -> CollisionPoint
shapeCollisionPoint (Circle p1 _)    (Circle p2 _)     = CollisionAngle angle
  where (px,py) = p2 ^-^ p1
        angle   = atan2 py px
shapeCollisionPoint (Circle _ _)     (SemiPlane _ s2)  = CollisionSide s2
shapeCollisionPoint (Circle p1 s1)   (Rectangle p2 s2) =
   velCollitionSide $ fromJust $ responseCircleAABB (p1, s1) (rectangleToCentre (p2, s2))

shapeCollisionPoint (SemiPlane _ s1)  (Circle _ _ )     = CollisionSide (oppositeSide s1)
shapeCollisionPoint (SemiPlane _ _)   (SemiPlane _ s2)  = CollisionSide s2
shapeCollisionPoint p@(SemiPlane _ _) r@(Rectangle _ _) = let CollisionSide side = shapeCollisionPoint r p
                                                          in CollisionSide (oppositeSide side)

shapeCollisionPoint r@(Rectangle _ _) c@(Circle _ _)      = let CollisionSide side = shapeCollisionPoint c r
                                                            in CollisionSide (oppositeSide side)
shapeCollisionPoint r@(Rectangle _ _) (SemiPlane p2 s2)   = let (p2', s2') = semiplaneRectangle p2 s2
                                                            in shapeCollisionPoint r (Rectangle p2' s2')
shapeCollisionPoint (Rectangle p1 s1) (Rectangle p2 s2) =
   velCollitionSide $ fromJust $ responseAABB2 (p1, s1) (p2, s2)

velCollitionSide (vx, vy)
  | vx < 0 && abs vx > abs vy = CollisionSide RightSide
  | vx > 0 && abs vx > abs vy = CollisionSide LeftSide
  | vy > 0 && abs vx < abs vy = CollisionSide TopSide
  --  -- | vy > 0 && abs vx < abs vy
  | otherwise                 = CollisionSide BottomSide

detectCollision :: (PhysicalObject o k Shape) => o -> o -> Maybe (Collision k)
detectCollision obj1 obj2
  | overlap obj1 obj2
  = case (physObjectShape obj1, physObjectShape obj2) of
      (Circle _ _, Circle _ _) ->
         if vrn < 0
           then Just response
           else Nothing
      _ -> Just response
  | otherwise = Nothing

 where response  = collisionResponseObj obj1 obj2
       relativeP = physObjectPos obj1 ^-^ physObjectPos obj2
       relativeV = physObjectVel obj1 ^-^ physObjectVel obj2
       -- If the inner product between the relative position and velocity
       -- is negative, then the two objects are approaching each other.
       -- Note that there is no collision if vrn = 0. This could be
       -- because the objects are at the same position and thus cannot get
       -- any closer. Or because their relative velocity is 0 and thus are
       -- not approaching for that reason. This, if there *is* a collision,
       -- then we know that both the relative position and velocity is non 0,
       -- and it is safe to e.g. normalize the relative position as is done
       -- in "correctVel" below.
       vrn       = relativeV `dot` relativeP

       -- HN 2016-04-26: Old code: Problematic if same positions! But all we
       -- want to know here is if the objects are approaching each other.
       -- For this, all that matters is the sign of the inner product. There
       -- is no need to normalize the relative position!
       --
       -- colNormal = normalize (physObjectPos obj1 ^-^ physObjectPos obj2)
       -- relativeV = physObjectVel obj1 ^-^ physObjectVel obj2
       -- vrn       = relativeV `dot` colNormal

overlap :: PhysicalObject o k Shape => o -> o -> Bool
overlap obj1 obj2 =
  overlapShape (physObjectShape obj1) (physObjectShape obj2)

collisionPoint :: PhysicalObject o k Shape => o -> o -> CollisionPoint
collisionPoint obj1 obj2 =
  shapeCollisionPoint (physObjectShape obj1) (physObjectShape obj2)

collisionResponseObj :: PhysicalObject o k Shape => o -> o -> Collision k
collisionResponseObj o1 o2 = Collision $
    map objectToCollision [(o1, collisionPt, o2), (o2, collisionPt', o1)]
  where
    collisionPt  = collisionPoint o1 o2
    collisionPt' = collisionPoint o2 o1

    objectToCollision (o,pt,o') =
      ( physObjectId o
      , correctVel (physObjectPos o) (physObjectPos o')
                   (physObjectVel o) (physObjectVel o')
                   pt (physObjectElas o)
      )

correctVel :: Pos2D -> Pos2D -> Vel2D -> Vel2D -> CollisionPoint -> Double -> Vel2D
-- Specialised cases: just more optimal execution
correctVel _p1 _p2 v1      _          _                           0 = v1
-- Collision against a wall
correctVel _p1 _p2 (v1x,v1y) _          (CollisionSide  TopSide)    e = (e * v1x, e * ensurePos v1y)
correctVel _p1 _p2 (v1x,v1y) _          (CollisionSide  BottomSide) e = (e * v1x, e * ensureNeg v1y)
correctVel _p1 _p2 (v1x,v1y) _          (CollisionSide  LeftSide)   e = (e * ensurePos v1x, e * v1y)
correctVel _p1 _p2 (v1x,v1y) _          (CollisionSide  RightSide)  e = (e * ensureNeg v1x, e * v1y)
-- General case
correctVel p1 p2 (v1x,v1y) (v2x, v2y) (CollisionAngle _) e = (v1x, v1y) ^+^ ((e * j) *^ colNormal)
  where colNormal = normalize (p1 ^-^ p2)
        relativeV = (v1x,v1y) ^-^ (v2x,v2y)
        vrn       = relativeV `dot` colNormal
        j         = (-1) *^ vrn / (colNormal `dot` colNormal)

