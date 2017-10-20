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
module Game.Physics.GameCollisions where

import           Data.Foldable
import           Prelude   hiding (concatMap)
import           Data.List hiding (concatMap)
import           Data.Maybe
import           Physics.TwoDimensions.PhysicalObjects
import           Game.Physics.Collisions

import qualified Game.Physics.Collisions      as C
import           Game.Physics.Shapes

-- | Given a list of objects, it detects all the collisions between them.
--
-- Note: this is a simple n*m-complex algorithm, with n the
-- number of objects and m the number of moving objects (right now,
-- only 2).
--
detectCollisions :: Foldable t => (Eq n , PhysicalObject o n Shape) => t o -> Collisions n
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
detectCollisions' :: (Foldable t, Foldable u) => (Eq n, PhysicalObject o n Shape) => t o -> u o -> [Collision n]
detectCollisions' objsT ms = concatMap (detectCollisions'' objsT) ms

-- | Detect collisions between one specific moving object and every existing
-- object. Each collision is idependent of the rest (which is not necessarily
-- what should happen, but since the transformed velocities are eventually
-- added, there isn't much difference in the end).
detectCollisions'' :: Foldable t => (Eq n, PhysicalObject o n Shape) => t o -> o -> [Collision n]
detectCollisions'' objsT m = concatMap (detectCollisions''' m) (toList objsT)

-- | Detect a possible collision between two objects. Uses the object's keys to
-- distinguish them. Uses the basic 'Object'-based 'detectCollision' to
-- determine whether the two objects do collide.
detectCollisions''' :: (Eq n, PhysicalObject o n Shape) => o -> o -> [Collision n]
detectCollisions''' m o
 | physObjectId m == physObjectId o = []    -- Same object -> no collision
 | otherwise                        = maybeToList (detectCollision m o)
