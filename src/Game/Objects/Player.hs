{-# LANGUAGE Arrows     #-}
{-# LANGUAGE MultiWayIf #-}
-- | This module defines the game player.
--
-- Objects are represented as Signal Functions as well ('ObjectSF'). In this
-- game we are introducing a novel construct, named 'AliveObject' in this
-- a particular instance of a 'ListSF'. A 'ListSF' is a signal function that
-- can die, or can produce more signal functions of the same type.
-- 
-- Each object is responsible for itself, but it cannot affect others:
-- objects can watch others, depend on others and react to them, but they
-- cannot /send a message/ or eliminate other objects. 
--
-- You may want to read the basic definition of 'Controller' and 'ObjectSF'
-- before you attempt to go through this module.
--
module Game.Objects.Player where

-- External imports
import Prelude hiding ((.))
import Control.Category ((.))
import Data.Maybe
import FRP.Yampa
import FRP.Yampa.Extra

-- General-purpose internal imports
import Data.Ord.Extra

import Physics.CollisionEngine
import Physics.TwoDimensions.Dimensions
import Physics.TwoDimensions.PhysicalObjects as Collisions

-- Internal iports
import Game.Constants
import Game.Input
import Game.Objects
import Game.ObjectSF

-- | A player with a given it, lives, position and initial vulnerability.
player :: ObjectName -> Int -> Pos2D -> Bool -> AliveObject
player name lives p0 vul = ListSF $ proc i -> do
  (ppos, pvel) <- playerMovement name p0 -< i

  let state = playerState (userInput i)

  -- newF1  <- isEvent ^<< edge                          -< controllerClick (userInput i)
  -- uniqId <- (\t -> "fire" ++ name ++ show t) ^<< time -< ()
  -- let newF1Arrows = [ fire uniqId (fst ppos, 0) False
  --                   | newF1 ]

  newF1Arrows <- playerGun name -< (i, ppos)

  -- Dead?
  let hitByBall = not $ null
                $ collisionMask (name, Player) (collisionObjectKind Ball)
                $ collisions i

  vulnerable <- alwaysForward $ 
                  switch (constant vul &&& after 2 ())
                         (\_ -> constant True) -< ()

  dead <- isEvent ^<< edge -< hitByBall && vulnerable

  let newPlayer   = [ player name (lives-1) p0 False
                    | dead  && lives > 0 ]

  dt <- deltas -< ()

  energy <- loopPre 5 (arr (dup . max 0 . min 5 . sumTime)) -< dt
  --  max 0 (min 5 (round (fromIntegral (playerEnergyObjs ol) + dt)))

  -- Final player
  returnA -< (Object { objectName           = name
                     , objectKind           = Player
                     , objectProperties     = PlayerProps state lives vulnerable (round energy)
                     , objectPos            = ppos
                     , objectVel            = pvel
                     , canCauseCollisions   = True
                     , collisionEnergy      = 1
                     }
             , dead
             , newF1Arrows ++ newPlayer)

  where 
    sumTime :: (DTime, DTime) -> DTime
    sumTime = uncurry (+)

-- | Movement of a player around the screen.
playerMovement :: ObjectName -> Pos2D -> SF ObjectInput (Pos2D, Vel2D)
playerMovement pid p0 = proc i -> do
  -- Obtain velocity based on state and input, and obtain
  -- velocity delta to be applied to the position.
  v  <- repeatSF getVelocity PlayerStand -< userInput i

  let collisionsWithBlocks :: Collisions.Collisions (ObjectName, ObjectKind)
      collisionsWithBlocks = filter onlyBlocks (collisions i)

      onlyBlocks :: Collisions.Collision (ObjectName, ObjectKind) -> Bool
      onlyBlocks (Collision cdata) = any (playerCollisionElem . fst) cdata

      playerCollisionElem s = isBlockId s || isWallId s
      isBlockId = collisionObjectKind Block
      isWallId  = collisionObjectKind Side

  let ev = changedVelocity (pid, Player) collisionsWithBlocks
      vc = fromMaybe v ev

  (px,py) <- (p0 ^+^) ^<< alwaysForward integral -< vc

  -- Calculate actual velocity based on corrected/capped position
  v' <- derivative -< (px, py)

  returnA -< ((px, py), v')

 where

   capPlayerPos (px, py) = (px', py')
     where px' = inRange (0, width - playerWidth)  px
           py' = inRange (0, height - playerHeight) py

   getVelocity :: PlayerState -> SF Controller (Vel2D, Event PlayerState)
   getVelocity pstate = stateVel pstate &&& stateChanged pstate

   stateVel :: PlayerState -> SF a Vel2D
   stateVel PlayerLeft     = constant (-playerSpeed, 0)
   stateVel PlayerRight    = constant (playerSpeed,  0)
   stateVel PlayerStand    = constant (0,            0)
   stateVel PlayerShooting = constant (0,            0)

   stateChanged :: PlayerState -> SF Controller (Event PlayerState)
   stateChanged oldState = playerState ^>> ifDiff oldState

-- | State of the player based in user input.
playerState :: Controller -> PlayerState
playerState controller
  | controllerClick controller = PlayerShooting
  | controllerLeft  controller = PlayerLeft
  | controllerRight controller = PlayerRight
  | otherwise                  = PlayerStand

-- ** Guns

-- | The player's gun. Guns can fire shots, so guns may include
--   more than one object.
playerGun :: ObjectName -> SF (ObjectInput, Pos2D) [AliveObject]
playerGun = singleShotGun
  -- To switch between different kinds of guns
  -- playerGun name = switch
  --   (singleShotGun name &&& after 5 ())
  --   (\_ -> multiShotGun name)

-- | Gun that can be fired once until the bullet hits the wall or a ball, and
--   then can be fired again.
singleShotGun :: ObjectName -> SF (ObjectInput, Pos2D) [AliveObject]
singleShotGun name = revSwitch (constant [] &&& gunFired name)
                               (\fireLSF -> blockedGun name fireLSF)

-- | Gun that can be fired multiple times.
multiShotGun :: ObjectName -> SF (ObjectInput, Pos2D) [AliveObject]
multiShotGun name = eventToList ^<< gunFired name

-- | Possible event carrying a projectile, triggered when the
--   gun has been fired.
gunFired :: ObjectName -> SF (ObjectInput, Pos2D) (Event AliveObject)
gunFired name = proc (i, ppos) -> do
  -- Fire!!
  newF1  <- edge -< controllerClick (userInput i)
  uniqId <- (\t -> "bullet" ++ name ++ show t) ^<< time -< ()

  let newFire = bullet uniqId (fst ppos + playerWidth / 2, 0) False
  returnA -< newF1 `tag` newFire

-- | Gun that cannot be fired until the current bullet hits
--   the ceiling or a ball.
blockedGun :: ObjectName -> AliveObject -> SF (ObjectInput, Pos2D) [AliveObject]
blockedGun name fsf = revSwitch (([fsf] --> constant []) &&& bulletDead fsf)
                                (\_ -> singleShotGun name)
  where
    bulletDead fsf = proc (oi, _) -> do
      (_, b, _) <- listSF fsf -< oi
      justDied  <- edge       -< b
      returnA -< justDied

-- | Fire \/ arrows \/ bullets \/ projectiles. If the third argument is
--   'False', they die when they hit the top of the screen. If the third
--   argument is 'True', they stuck for a while before they die.
bullet :: ObjectName -> Pos2D -> Bool -> AliveObject
bullet name (x0, y0) sticky = ListSF $ proc i -> do

  -- Calculate arrow tip
  yT <- (y0+) ^<< integral -< bulletSpeed
  let y = min height yT

  -- Delay death if the bullet is "sticky"
  hit <- revSwitch (never &&& bulletHitCeiling) (\_ -> stickyDeath sticky) -< y

  let hitBall  = bulletCollidedWithBall  name $ collisions i
  let hitBlock = bulletCollidedWithBlock name $ collisions i

  let dead = isEvent hit || hitBall || hitBlock

  let object = Object { objectName         = name
                      , objectKind         = Projectile
                      , objectProperties   = ProjectileProps
                      , objectPos          = (x0, y)
                      , objectVel          = (0, 0)
                      , canCauseCollisions = True
                      , collisionEnergy    = 0
                      }

  returnA -< (object, dead, [])

 where

   bulletHitCeiling = (>= height) ^>> edge
   bulletCollidedWithBall  bid = not . null . collisionMask (bid, Projectile) (collisionObjectKind Ball)
   bulletCollidedWithBlock bid = not . null . collisionMask (bid, Projectile) (collisionObjectKind Block)

   stickyDeath :: Bool -> SF a (Event ())
   stickyDeath True  = after 30 ()
   stickyDeath False = constant (Event ())
