{-# LANGUAGE FlexibleContexts #-}
-- | A very rudimentary collision system.
--
-- It compares every pair of objects, trying to determine if there is a
-- collision between the two of them.
--
-- NOTE: In order to minimize the number of comparisons, only moving objects
-- are tested (against every game object). That's only 2 objects right now
-- (making it almost linear in complexity), but it could easily grow and become
-- too slow.
--
module Physics.CollisionEngine where

import           Data.Foldable
import           Prelude   hiding (concatMap)
import           Data.List hiding (concatMap)
import           Data.Maybe
import           Physics.TwoDimensions.Dimensions
import           Physics.TwoDimensions.PhysicalObjects as C

-- | Given a list of objects, it detects all the collisions between them.
--
-- Note: this is a simple n*m-complex algorithm, with n the
-- number of objects and m the number of moving objects (right now,
-- only 2).
--
detectCollisions :: Foldable t => (Eq n , PhysicalObject o n s) => t o -> Collisions n
detectCollisions objsT = flattened
  where -- Eliminate empty collision sets
        -- TODO: why is this really necessary?
        flattened = filter collisionNotEmpty collisions

        -- Detect collisions between moving objects and any other objects
        collisions = detectCollisions' objsT moving

        -- Partition the object space between moving and static objects
        (moving, _static) = partition physObjectCollides $ toList objsT

        -- Is the collision set empty?
        collisionNotEmpty (C.Collision n) = not (null n)

-- | Detect collisions between each moving object and
-- every other object.
detectCollisions' :: (Foldable t, Foldable u) => (Eq n, PhysicalObject o n s) => t o -> u o -> [Collision n]
detectCollisions' objsT ms = concatMap (detectCollisions'' objsT) ms

-- | Detect collisions between one specific moving object and every existing
-- object. Each collision is idependent of the rest (which is not necessarily
-- what should happen, but since the transformed velocities are eventually
-- added, there isn't much difference in the end).
detectCollisions'' :: Foldable t => (Eq n, PhysicalObject o n s) => t o -> o -> [Collision n]
detectCollisions'' objsT m = concatMap (detectCollisions''' m) (toList objsT)

-- | Detect a possible collision between two objects. Uses the object's keys to
-- distinguish them. Uses the basic 'Object'-based 'detectCollision' to
-- determine whether the two objects do collide.
detectCollisions''' :: (Eq n, PhysicalObject o n s) => o -> o -> [Collision n]
detectCollisions''' m o
 | physObjectId m == physObjectId o = []    -- Same object -> no collision
 | otherwise                        = maybeToList (physDetectCollision m o)

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
    []          -> Nothing
    (_, v') : _ -> Just v'
    -- vs       -> Just (foldl (^+^) (0,0) (map snd vs))

-- | True if the velocity of the object has been changed by any collision.
inCollision :: Eq n => n -> Collisions n -> Bool
inCollision name cs = isJust (changedVelocity name cs)

-- | True if the two objects are colliding with one another.
inCollisionWith :: Eq n => n -> n -> Collisions n -> Bool
inCollisionWith nm1 nm2 cs = any both cs
  where
    both (Collision nmvs) =
      any ((== nm1) . fst) nmvs && any ((== nm2) . fst) nmvs

-- * Apply an ID-based collision mask
collisionMask :: Eq id
              => id -> (id -> Bool) -> Collisions id -> Collisions id
collisionMask cId mask = onCollisions ( filter (any (mask . fst))
                                      . filter (any ((== cId).fst))
                                      )

 where onCollisions :: ([[(id, Vel2D)]] -> [[(id, Vel2D)]])
                    -> Collisions id    -> Collisions id
       onCollisions f = map Collision . f . map collisionData
