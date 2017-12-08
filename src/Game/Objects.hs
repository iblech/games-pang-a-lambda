{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Game objects and collisions.
module Game.Objects where

import           Data.Maybe                                (listToMaybe)
import           Physics.TwoDimensions.Dimensions
import           Physics.TwoDimensions.PhysicalObjects                   as P
import           Physics.TwoDimensions.Shapes.BasicCirclesAABB
import qualified Physics.TwoDimensions.Shapes.BasicCirclesAABBCollisions as C
import           Physics.TwoDimensions.Side

import           Game.Constants

-- * Objects

-- | Object collection.
type Objects = [Object]

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

-- | Type for object id.
type ObjectName = String

-- | The kind of object and any size properties.
data ObjectKind = Ball
                | Player
                | Side
                | Projectile
                | Block
                -- -- | PowerUp PowerUp
  deriving (Show,Eq)

-- | Properties associated to each kind of object.
data ObjectProperties
  = BallProps       !Double -- radius
  | PlayerProps     !PlayerState !Int {- lives -} !Bool {- Vulnerable -} !Int {- energy -}
  | SideProps       !Side
  | ProjectileProps
  | BlockProps      !Size2D
  -- -- | PowerUpProps PowerUp
  deriving (Show,Eq)

-- | The state in which a player is. Can be standing, or shooting.
data PlayerState = PlayerRight
                 | PlayerLeft
                 | PlayerStand
                 | PlayerShooting
  deriving (Eq, Show)

-- | Energy left for the player, or zero if the object is not a player.
playerEnergy :: Object -> Int
playerEnergy o = case objectProperties o of
  p@(PlayerProps _ _ _ e) -> e
  _                       -> 0

-- | Object size. Partial function!
objectSize :: Object -> Size2D
objectSize object = case objectProperties object of
  (BallProps r)    -> let w = 2*r in (w, w)
  (PlayerProps {}) -> (playerWidth, playerHeight)
  (BlockProps s)   -> s

-- ** Distinguish objects by kind.
findPlayer :: Objects -> Maybe Object
findPlayer = listToMaybe . filter isPlayer

isBall :: Object -> Bool
isBall o = case objectKind o of
  Ball -> True
  _    -> False

isPlayer :: Object -> Bool
isPlayer o = case objectKind o of
  Player -> True
  _      -> False

-- * Physical properties

-- | Physical object definition of an 'Object'. We use AABBs and circles for
--   shapes.
instance PhysicalObject Object (String, ObjectKind) Shape where
  physObjectPos       = objectPos
  physObjectVel       = objectVel
  physObjectElas      = collisionEnergy
  physObjectShape     = objShape
  physObjectCollides  = canCauseCollisions
  physObjectId x      = (objectName x, objectKind x)
  physObjectUpdatePos = \o p -> o { objectPos = p }
  physObjectUpdateVel = \o v -> o { objectVel = v }
  physDetectCollision = C.detectCollision

-- | Collision shape of an object.
objShape :: Object -> Shape
objShape obj = case objectProperties obj of
  BallProps r        -> Circle p r
  SideProps s        -> SemiPlane p s
  PlayerProps {}     -> Rectangle p (playerWidth, playerHeight)
  ProjectileProps    -> Rectangle (px - 5, 0) (10, py)
  BlockProps s@(w,h) -> Rectangle (px, py) s
 where p@(px,py) = objectPos obj

-- ** Collisions

type Collision  = P.Collision  (ObjectName, ObjectKind)
type Collisions = P.Collisions (ObjectName, ObjectKind)

-- | Check if collision is of given type.
collisionObjectKind :: ObjectKind -> (ObjectName, ObjectKind) -> Bool
collisionObjectKind ok1 (_, ok2) = ok1 == ok2
