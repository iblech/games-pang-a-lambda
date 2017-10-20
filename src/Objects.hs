{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Game objects and collisions.
module Objects where

import Control.Arrow ((***))
import Data.Maybe (listToMaybe)
import FRP.Yampa.VectorSpace

import qualified Physics.TwoDimensions.Collisions      as C
import           Physics.TwoDimensions.Dimensions
import           Physics.TwoDimensions.PhysicalObjects
import           Physics.TwoDimensions.Shapes

import Constants

type Collision  = C.Collision  (ObjectName, ObjectKind)
type Collisions = C.Collisions (ObjectName, ObjectKind)

-- * Objects

type Objects = [Object]
type ObjectName = String

-- | Objects have logical properties (ID, kind, dead, hit), shape properties
-- (kind), physical properties (kind, pos, vel, acc) and collision properties
-- (hit, 'canCauseCollisions', energy, displaced).
--
-- The properties need to agree with the kind. The kind is necessary to
-- avoid using string matching on the name to determine the object kind.
data Object = Object { objectName           :: !ObjectName
                     , objectKind           :: !ObjectKind
                     , objectProperties     :: !ObjectProperties
                     , objectPos            :: !Pos2D
                     , objectVel            :: !Vel2D
                     , canCauseCollisions   :: !Bool
                     , collisionEnergy      :: !Double
                     }
 deriving (Show)

findPlayer = listToMaybe . filter isPlayer

isBall :: Object -> Bool
isBall o = case objectKind o of
  Ball -> True
  _    -> False

isPlayer :: Object -> Bool
isPlayer o = case objectKind o of
  Player -> True
  _      -> False

-- | The kind of object and any size properties.
data ObjectKind = Ball
                | Player
                | Side
                | Projectile
                | Block
                -- -- | PowerUp PowerUp
  deriving (Show,Eq)

data ObjectProperties
  = BallProps       !Double -- radius
  | PlayerProps     !PlayerState !Int {- lives -} !Bool {- Vulnerable -} !Int {- energy -}
  | SideProps       !Side
  | ProjectileProps
  | BlockProps      !Size2D
  -- -- | PowerUpProps PowerUp
  deriving (Show,Eq)

data PlayerState = PlayerRight
                 | PlayerLeft
                 | PlayerStand
                 | PlayerShooting
  deriving (Eq, Show)

playerEnergy :: Object -> Int
playerEnergy o = case objectProperties o of
  p@(PlayerProps _ _ _ e) -> e
  _                       -> 0

-- Partial function!
objectSize :: Object -> Size2D
objectSize object = case objectProperties object of
  (BallProps r)    -> let w = 2*r in (w, w)
  (PlayerProps {}) -> (playerWidth, playerHeight)
  (BlockProps s)   -> s

instance PhysicalObject Object (String, ObjectKind) Shape where
  physObjectPos       = objectPos
  physObjectVel       = objectVel
  physObjectElas      = collisionEnergy
  physObjectShape     = objShape
  physObjectCollides  = canCauseCollisions
  physObjectId x      = (objectName x, objectKind x)
  physObjectUpdatePos = \o p -> o { objectPos = p }
  physObjectUpdateVel = \o v -> o { objectVel = v }

objShape :: Object -> Shape
objShape obj = case objectProperties obj of
  BallProps r        -> Circle p r
  SideProps s        -> SemiPlane p s
  PlayerProps {}     -> Rectangle p (playerWidth, playerHeight)
  ProjectileProps    -> Rectangle (px - 5, 0) (10, py)
  BlockProps s@(w,h) -> Rectangle (px, py) s
 where p@(px,py) = objectPos obj
