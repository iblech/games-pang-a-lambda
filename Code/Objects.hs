{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Game objects and collisions.
module Objects where

import Control.Arrow ((***))
import FRP.Yampa.VectorSpace

import qualified Physics.TwoDimensions.Collisions      as C
import           Physics.TwoDimensions.Dimensions
import           Physics.TwoDimensions.PhysicalObjects
import           Physics.TwoDimensions.Shapes

import Constants

type Collision  = C.Collision ObjectName
type Collisions = C.Collisions ObjectName

-- * Objects

type Objects = [Object]
type ObjectName = String

-- | Objects have logical properties (ID, kind, dead, hit), shape properties
-- (kind), physical properties (kind, pos, vel, acc) and collision properties
-- (hit, 'canCauseCollisions', energy, displaced).
data Object = Object { objectName           :: !ObjectName
                     , objectKind           :: !ObjectKind
                     , objectPos            :: !Pos2D
                     , objectVel            :: !Vel2D
                     , canCauseCollisions   :: !Bool
                     , collisionEnergy      :: !Double
                     }
 deriving (Show)

-- | The kind of object and any size properties.
data ObjectKind = Ball   Double -- radius
                | Player PlayerState
                | Side   Side
                | Projectile
                -- | PowerUp PowerUp
                -- | Block Size
  deriving (Show,Eq)

data PlayerState = PlayerRight
                 | PlayerLeft
                 | PlayerStand
  deriving (Eq, Show)

-- Partial function. Object has size.
objectTopLevelCorner :: Object -> Pos2D
objectTopLevelCorner object = objectPos object ^-^ half (objectSize object)
  where half = let h = (/2) in (h *** h)

-- Partial function!
objectSize :: Object -> Size2D
objectSize object = case objectKind object of
  (Ball r)    -> let w = 2*r in (w, w)
  (Player {}) -> (playerWidth, playerHeight)

instance PhysicalObject Object String Shape where
  physObjectPos       = objectPos
  physObjectVel       = objectVel
  physObjectElas      = collisionEnergy
  physObjectShape     = objShape
  physObjectCollides  = canCauseCollisions
  physObjectId        = objectName
  physObjectUpdatePos = \o p -> o { objectPos = p }
  physObjectUpdateVel = \o v -> o { objectVel = v }

objShape :: Object -> Shape
objShape obj = case objectKind obj of
  -- Ball r -> Circle p r
  Ball r -> Rectangle (px -r, py - r) (r*2, r*2)
  Side s -> SemiPlane p s
  Player _ -> Rectangle p (playerWidth, playerHeight)
  Projectile -> Rectangle (px - 5, 0) (10, py)
 where p@(px,py) = objectPos obj
