{-# LANGUAGE FlexibleContexts       #-}
-- | A trivial collision subsystem.
--
-- Based on the physics module, it determines the side of collision
-- between shapes.
module Physics.TwoDimensions.Collisions where

import Collisions
import Shapes
import Control.Monad
import Data.Extra.Num
import Data.Maybe
import FRP.Yampa.VectorSpace
import Physics.TwoDimensions.Dimensions
import Physics.TwoDimensions.PhysicalObjects
import Physics.TwoDimensions.Shapes

-- * Collision points
data CollisionPoint = CollisionSide  Side
                    | CollisionAngle Double

-- | Calculates the collision side of a shape
-- that collides against another.
--
-- PRE: the shapes do collide. Use 'overlapShape' to check.
shapeCollisionPoint :: Shape -> Shape -> CollisionPoint
shapeCollisionPoint (Circle p1 _) (Circle p2 _) =
  -- | p1x =~ p2x && p1y >  p2y = CollisionAngle (- pi / 2)
  -- | p1x =~ p2x && p1y <= p2y = CollisionAngle (pi / 2)
  -- | otherwise                =
   CollisionAngle angle
  where (px,py) = p2 ^-^ p1
        angle   = atan2 py px
shapeCollisionPoint (Circle _ _)     (SemiPlane _ s2)  = CollisionSide s2
shapeCollisionPoint (SemiPlane _ s1) (Circle _ _ )     = CollisionSide (oppositeSide s1)
shapeCollisionPoint (SemiPlane _ _)  (SemiPlane _ s2)  = CollisionSide s2
shapeCollisionPoint (Circle p1 s1)   (Rectangle p2 s2) =
   velCollitionSide $ fromJust $ responseCircleAABB (p1, s1) (rectangleToCentre (p2, s2))
shapeCollisionPoint r@(Rectangle _ _) c@(Circle _ _)      = let CollisionSide side = shapeCollisionPoint c r
                                                            in CollisionSide (oppositeSide side)
shapeCollisionPoint r@(Rectangle _ _) (SemiPlane p2 s2)   = let (p2', s2') = semiplaneRectangle p2 s2
                                                            in shapeCollisionPoint r (Rectangle p2' s2')
shapeCollisionPoint p@(SemiPlane _ _) r@(Rectangle _ _)   = let CollisionSide side = shapeCollisionPoint r p
                                                            in CollisionSide (oppositeSide side)
shapeCollisionPoint (Rectangle p1 s1) (Rectangle p2 s2) =
   velCollitionSide $ fromJust $ responseAABB2 (p1, s1) (p2, s2)

velCollitionSide (vx, vy)
  | vx < 0 && abs vx > abs vy = CollisionSide RightSide
  | vx > 0 && abs vx > abs vy = CollisionSide LeftSide
  | vy > 0 && abs vx < abs vy = CollisionSide TopSide
  -- | vy > 0 && abs vx < abs vy
  | otherwise                 = CollisionSide BottomSide

-- * Collisions
type Collisions k = [Collision k]

-- | A collision is a list of objects that collided, plus their velocities as
-- modified by the collision.
--
-- Take into account that the same object could take part in several
-- simultaneous collitions, so these velocities should be added (per object).
data Collision k = Collision
  { collisionData :: [(k, Vel2D)] } -- ObjectId x Velocity
 deriving Show

-- -- | Detects a collision between one object and another.
-- detectCollision :: (PhysicalObject o k Shape) => o -> o -> Maybe (Collision k)
-- detectCollision obj1 obj2 =
--   case (physObjectShape obj1, physObjectShape obj2) of
--     (Circle p1 r1, Circle p2 r2) -> do
--       -- In the maybe monad
--       guard (overlapsCircle2 (p1,r1) (p2,r2))
--       vd1 <- responseCircle2 (p1, r1) (p2, r2)
--       vd2 <- return $ (\(vx, vy) -> (-vx, -vy)) vd1
--       -- vd2 <- responseCircle2 (p2, r2) (p1, r1)
--       return $ Collision [ (physObjectId obj1, vd1)
--                          , (physObjectId obj2, vd2)
--                          ]
--     (Circle (p1x,p1y) s1, SemiPlane (px,py) side) -> do
--       case side of
--          LeftSide   -> guard (p1x - s1 <= px && fst (physObjectVel obj1) < 0)
--          RightSide  -> guard (p1x + s1 >= px && fst (physObjectVel obj1) > 0)
--          TopSide    -> guard (p1y - s1 <= py && snd (physObjectVel obj1) < 0)
--          BottomSide -> guard (p1y + s1 >= py && snd (physObjectVel obj1) > 0)
--       vd <- case side of
--               LeftSide   -> return $ (\(vx,vy) -> ((-1) * vx, 0)) $ physObjectVel obj1
--               RightSide  -> return $ (\(vx,vy) -> ((-1) * vx, 0)) $ physObjectVel obj1
--               TopSide    -> return $ (\(vx,vy) -> (0, (-1) * vy)) $ physObjectVel obj1
--               BottomSide -> return $ (\(vx,vy) -> (0, (-1) * vy)) $ physObjectVel obj1
--       return $ Collision [ (physObjectId obj1, vd)
--                          , (physObjectId obj2, (0,0))
--                          ]
--     (Circle p1 r1, Rectangle p2 s2) -> do
--       -- In the maybe monad
--       guard (circleAABBOverlap (p1,r1) (p2,s2))
--       vd1 <- responseCircleAABB (p1, r1) (p2, s2)
--       vd2 <- return $ (\(vx, vy) -> (-vx, -vy)) vd1
--       return $ Collision [ (physObjectId obj1, vd1)
--                          , (physObjectId obj2, vd2)
--                          ]
--     (Rectangle p1 s1, Rectangle p2 s2) -> do
--       guard (overlapsAABB2 (p1, s1) (p2, s2))
--       vd1 <- responseAABB2 (p1, s1) (p2, s2)
--       vd2 <- return $ (\(vx, vy) -> (-vx, -vy)) vd1
--       return $ Collision [ (physObjectId obj1, vd1)
--                          , (physObjectId obj2, vd2)
--                          ]
--     (SemiPlane _ _, Circle _ _) -> detectCollision obj2 obj1
--     (Rectangle _ _, Circle _ _) -> detectCollision obj2 obj1
--     _ -> Nothing



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
       colNormal = normalize (physObjectPos obj1 ^-^ physObjectPos obj2)
       relativeV = physObjectVel obj1 ^-^ physObjectVel obj2
       vrn       = relativeV `dot` colNormal

overlap :: PhysicalObject o k Shape => o -> o -> Bool
overlap obj1 obj2 =
  overlapShape (physObjectShape obj1) (physObjectShape obj2)

collisionPoint :: PhysicalObject o k Shape => o -> o -> CollisionPoint
collisionPoint obj1 obj2 =
  shapeCollisionPoint (physObjectShape obj1) (physObjectShape obj2)

collisionResponseObj :: PhysicalObject o k Shape => o -> o -> Collision k
collisionResponseObj o1 o2 = Collision $
    map objectToCollision [(o1, collisionPt, o2), (o2, collisionPt', o1)]
  where collisionPt  = collisionPoint o1 o2
        collisionPt' = collisionPoint o2 o1
        objectToCollision (o,pt,o') =
          (physObjectId o,
           correctVel (physObjectPos o) (physObjectPos o')
                      (physObjectVel o) (physObjectVel o')
                      pt (physObjectElas o))

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

-- | Return the new velocity as changed by the collection of collisions.
--
-- HN 2014-09-07: New interface to collision detection.
--
-- The assumption is that collision detection happens globally and that the
-- changed velocity is figured out for each object involved in a collision
-- based on the properties of all objects involved in any specific interaction.
-- That may not be how it works now, but the interface means it could work
-- that way. Even more physical might be to figure out the impulsive force
-- acting on each object.
--
-- However, the whole collision infrastructure should be revisited.
--
-- - Statefulness ("edge") might make it more robust.
--
-- - Think through how collision events are going to be communicated
--   to the objects themselves. Maybe an input event is the natural
--   thing to do. Except then we have to be careful to avoid switching
--   again immediately after one switch.
--
-- - Should try to avoid n^2 checks. Maybe some kind of quad-trees?
--   Maybe spawning a stateful collision detector when two objects are
--   getting close? Cf. the old tail-gating approach.
-- - Maybe a collision should also carry the identity of the object
--   one collieded with to facilitate impl. of "inCollisionWith".
--
changedVelocity :: Eq n => n -> Collisions n -> Maybe Vel2D
changedVelocity name cs =
    case concatMap (filter ((== name) . fst) . collisionData) cs of
        [] -> Nothing
        -- vs -> Just (foldl (^+^) (0,0) (map snd vs))
        (_, v') : _ -> Just v'

-- | True if the velocity of the object has been changed by any collision.
inCollision :: Eq n => n -> Collisions n -> Bool
inCollision name cs = isJust (changedVelocity name cs)

-- | True if the two objects are colliding with one another.
inCollisionWith :: Eq n => n -> n -> Collisions n -> Bool
inCollisionWith nm1 nm2 cs = any both cs
    where
        both (Collision nmvs) =
            any ((== nm1) . fst) nmvs
            && any ((== nm2) . fst) nmvs

-- * Apply an ID-based collision mask
collisionMask :: Eq id
              => id -> (id -> Bool) -> Collisions id -> Collisions id
collisionMask cId mask = onCollisions ( filter (any (mask . fst))
                                      . filter (any ((== cId).fst))
                                      )

 where onCollisions :: ([[(id, Vel2D)]] -> [[(id, Vel2D)]])
                    -> Collisions id    -> Collisions id
       onCollisions f = map Collision . f . map collisionData

