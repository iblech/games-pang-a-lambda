{-# LANGUAGE Arrows     #-}
{-# LANGUAGE MultiWayIf #-}
-- | This module defines the game enemies, or balls.
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
module Game.Objects.Balls where

-- External imports
import Prelude hiding ((.))
import Control.Category ((.))
import FRP.Yampa
import FRP.Yampa.Extra
import FRP.Yampa.Switches

-- General-purpose internal imports
import Data.Extra.VectorSpace
import Physics.TwoDimensions.Collisions       as Collisions
import Physics.TwoDimensions.Dimensions
import Physics.TwoDimensions.PhysicalObjects

-- Internal iports
import Game.Constants
import Game.Input
import Game.Objects
import Game.ObjectSF
import Game.Time

-- ** Balls

-- | A ball that splits in two when hit unless it is too small.
splittingBall :: ObjectName -> Double -> Pos2D -> Vel2D -> AliveObject
splittingBall bid size p0 v0 = ListSF $ timeTransformSF timeProgressionHalt $ proc i -> do

    -- Default, just bouncing behaviour
    bo <- bouncingBall bid size p0 v0 -< i

    -- Hit fire? If so, it should split
    click <- edge <<^ ballIsHit bid -< collisions i
    let shouldSplit = isEvent click

    -- We need two unique IDs so that collisions work
    t <- localTime -< ()
    let offspringIDL = bid ++ show t ++ "L"
        offspringIDR = bid ++ show t ++ "R"

    let enforceYPositive (x,y) = (x, abs y)

    -- Position and velocity of new offspring
    let bpos = physObjectPos bo
        bvel = enforceYPositive $ physObjectVel bo
        ovel = enforceYPositive $ (\(vx,vy) -> (-vx, vy)) bvel

    -- Offspring size, unless this ball is too small to split
    let tooSmall      = size <= (ballWidth / 8)
    let offspringSize = size / 2

    -- Calculate offspring, if any
    let offspringL = splittingBall offspringIDL offspringSize bpos bvel
        offspringR = splittingBall offspringIDR offspringSize bpos ovel
        offspring  = if shouldSplit && not tooSmall
                      then [ offspringL, offspringR ]
                      else []

    -- If it splits, we just remove this one
    let dead = shouldSplit

    returnA -< (bo, dead, offspring)

  where

    -- | Determine if a given fall has been hit by a bullet.
    ballIsHit :: ObjectName -> Game.Objects.Collisions -> Bool
    ballIsHit bid = not . null . collisionMask (bid, Ball) (collisionObjectKind Projectile)

-- | A bouncing ball that moves freely until there is a collision, then bounces
-- and goes on and on.
--
-- This SF needs an initial position and velocity. Every time there is a
-- bounce, it takes a snapshot of the point of collision and corrected
-- velocity, and starts again.
--
bouncingBall :: ObjectName -> Double -> Pos2D -> Vel2D -> ObjectSF
bouncingBall bid size p0 v0 = repeatRevSF (progressAndBounce bid size) (p0, v0)

-- | Calculate the future tentative position, and bounce if necessary. Pass on
-- snapshot of ball position and velocity if bouncing.
progressAndBounce :: ObjectName -> Double -> (Pos2D, Vel2D)
                  -> SF ObjectInput (Object, Event (Pos2D, Vel2D))
progressAndBounce bid size (p0, v0) = proc i -> do

  -- Position of the ball, starting from p0 with velicity v0, since the
  -- time of last switching (or being fired, whatever happened last)
  -- provided that no obstacles are encountered.
  o <- freeBall bid size p0 v0 -< i

  -- The ballBounce needs the ball SF' input (which has knowledge of
  -- collisions), so we carry it parallely to the tentative new
  -- positions, and then use it to detect when it's time to bounce
  b <- ballBounce bid -< (i, o)

  returnA -< (o, b)

-- | Detect if the ball must bounce and, if so, take a snapshot of the object's
-- current position and velocity.
--
-- It proceeds by detecting whether any collision affects the ball's velocity,
-- and outputs a snapshot of the object position and the corrected velocity if
-- necessary.

-- NOTE: To avoid infinite loops when switching, the initial input is discarded
-- and never causes a bounce. Careful: this prevents the ball from bouncing
-- immediately after creation, which may or may not be what we want.
ballBounce :: ObjectName -> SF (ObjectInput, Object) (Event (Pos2D, Vel2D))
ballBounce bid = noEvent --> proc (ObjectInput ci cs, o) -> do
  -- HN 2014-09-07: With the present strategy, need to be able to
  -- detect an event directly after
  -- ev <- edgeJust -< changedVelocity "ball" cs
  let collisionsWithoutBalls = filter (not . allBalls) cs
      allBalls (Collision cdata) = all (collisionObjectKind Ball . fst) cdata

  let collisionsWithoutPlayer = filter (not . anyPlayer)
                                 collisionsWithoutBalls
      anyPlayer (Collision cdata) = any (collisionObjectKind Player . fst) cdata

  let ev = maybeToEvent (changedVelocity (bid, Ball) collisionsWithoutPlayer)
  returnA -< fmap (\v -> (objectPos o, v)) ev

-- | Position of the ball, starting from p0 with velicity v0, since the time of
-- last switching (that is, collision, or the beginning of time if never
-- switched before), provided that no obstacles are encountered.
freeBall :: ObjectName -> Double -> Pos2D -> Vel2D -> ObjectSF
freeBall name size p0 v0 = proc (ObjectInput ci cs) -> do

  -- Integrate acceleration, add initial velocity and cap speed. Resets both
  -- the initial velocity and the current velocity to (0,0) when the user
  -- presses the Halt key (hence the dependency on the controller input ci).
  vInit <- startAs v0 -< ci
  vel   <- vdiffSF    -< (vInit, (0, -1000.8), ci)

  -- Any free moving object behaves like this (but with
  -- acceleration. This should be in some FRP.NewtonianPhysics
  -- module)
  pos <- (p0 ^+^) ^<< integral -< vel

  let obj = Object { objectName           = name
                   , objectKind           = Ball
                   , objectProperties     = BallProps size
                   , objectPos            = pos
                   , objectVel            = vel
                   , canCauseCollisions   = True
                   , collisionEnergy      = 1
                   }

  returnA -< obj
 where
   -- Spike every time the user presses the Halt key
   restartCond = spikeOn (arr controllerStop)

   -- Calculate the velocity, restarting when the user
   -- requests it.
   vdiffSF = proc (iv, acc, ci) -> do
               -- Calculate velocity difference by integrating acceleration
               -- Reset calculation when user requests to stop balls
               vd <- restartOn (fst ^>> integral)
                               (snd ^>> restartCond) -< (acc, ci)

               -- Add initial velocity, and cap the result
               v <- arr (uncurry (^+^)) -< (iv, vd)
               let vFinal = limitNorm v (maxVNorm size)

               returnA -< vFinal

   -- Initial velocity, reset when the user requests it.
   startAs v0  = revSwitch (constant v0 &&& restartCond)
                           (\_ -> startAs (0,0))
