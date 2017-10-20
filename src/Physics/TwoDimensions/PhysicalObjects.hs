{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Physics.TwoDimensions.PhysicalObjects where

import Physics.TwoDimensions.Dimensions

class Eq b => PhysicalObject a b c | a -> b, a -> c where
  physObjectPos       :: a -> Pos2D
  physObjectVel       :: a -> Vel2D
  physObjectElas      :: a -> Double
  physObjectShape     :: a -> c
  physObjectCollides  :: a -> Bool
  physObjectId        :: a -> b
  physObjectUpdatePos :: a -> Pos2D -> a
  physObjectUpdateVel :: a -> Vel2D -> a
  physDetectCollision :: a -> a -> Maybe (Collision b)

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
