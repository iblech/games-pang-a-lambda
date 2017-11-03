{-# LANGUAGE Arrows     #-}
-- | This module defines game blocks.
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
module Game.Objects.Blocks where

-- External imports
import Prelude hiding (id)
import Debug.Trace
import FRP.Yampa
import FRP.Extra.Yampa

-- General-purpose internal imports
import Physics.Oscillator
import Physics.TwoDimensions.Dimensions

-- Internal iports
import Game.Objects
import Game.ObjectSF
import Game.Time

-- ** Blocks

-- | Static block builder, given a name, a size and its base
-- position.
staticBlock :: ObjectName -> Pos2D -> Size2D -> AliveObject
staticBlock name pos size = ListSF $ timeTransformSF timeProgressionHalt $ constant
  (Object { objectName           = name
          , objectKind           = Block
          , objectProperties     = BlockProps size
          , objectPos            = pos
          , objectVel            = (0,0)
          , canCauseCollisions   = False
          , collisionEnergy      = 0
          }, False, [])

-- | Moving block with an initial position and size, and horizontal and
-- vertical amplitude and periods. If an amplitude is /not/ zero, the block
-- moves along that dimension using a periodic oscillator (see 'osci').
oscillatingBlock :: ObjectName
                 -> Pos2D -> Size2D  -- Geometry
                 -> Double -> Double -- Horizontal oscillation amplitude and period
                 -> Double -> Double -- Vertical   oscillation amplitude and period
                 -> AliveObject
oscillatingBlock name (px, py) size hAmp hPeriod vAmp vPeriod = ListSF $ proc _ -> do
  px' <- vx -< px
  py' <- vy -< py
  returnA -< (Object { objectName           = name
                     , objectKind           = Block
                     , objectProperties     = BlockProps size
                     , objectPos            = (px', py')
                     , objectVel            = (0,0)
                     , canCauseCollisions   = True
                     , collisionEnergy      = 0
                     }, False, [])

 where

   -- To avoid errors, we check that the amplitude is non-zero, otherwise
   -- just pass the given position along.
   vx :: SF Double Double
   vx = if hAmp /= 0 then (px +) ^<< osci hAmp hPeriod else identity

   -- To avoid errors, we check that the amplitude is non-zero, otherwise
   -- just pass the given position along.
   vy :: SF Double Double
   vy = if vAmp /= 0 then (py +) ^<< osci vAmp vPeriod else identity

arcBlock :: ObjectName
         -> Pos2D -> Size2D  -- Geometry
         -> Double -> Double
         -> Double -> Double
         -> AliveObject
arcBlock name p0 size hAmp hPeriod vAmp vPeriod = ListSF $ proc _ -> do
  t <- time -< ()
  let x = hAmp * cos (2 * pi * t / hPeriod)
      y = vAmp * sin (2 * pi * t / vPeriod)
      p = p0 ^+^ (x,y)
  returnA -< (Object { objectName           = name
                     , objectKind           = Block
                     , objectProperties     = BlockProps size
                     , objectPos            = p
                     , objectVel            = (0,0)
                     , canCauseCollisions   = True
                     , collisionEnergy      = 0
                     }, False, [])

-- | Creates a block that slides sideways, waits for some time, slides back,
--   waits again, and repeats.
slidingBlock :: ObjectName
             -> Pos2D -> Size2D  -- Geometry
             -> Double           -- horizontal displacement
             -> Time             -- time to move
             -> Time             -- time to wait
             -> AliveObject
slidingBlock name (x0, y) size hDisplacement moveDuration waitDuration =
  ListSF $ proc _ -> do

    -- Proportion is a number from 0 to 1. It increases for some time, stays at
    -- one, and goes back.
    prop <- strangeClock moveDuration waitDuration -< ()
  
    -- Calculate position using the time-based proportion
    let x = x0 + prop * hDisplacement
        p = (x, y)
  
    returnA -< (Object { objectName           = name
                       , objectKind           = Block
                       , objectProperties     = BlockProps size
                       , objectPos            = p
                       , objectVel            = (0,0)
                       , canCauseCollisions   = True
                       , collisionEnergy      = 0
                       }, False, [])
  
  where
    -- | Signal that goes from zero to one in 'dur' seconds, stays at one
    --   for 'wait' seconds, repeats in the opposite direction.
    strangeClock :: Time -> Time -> SF () Time
    strangeClock dur wait =
        switch (proportionMove     >>> (identity   &&& isOne))  $ \_ ->
        switch (proportionWait     >>> (constant 1 &&& isOne))  $ \_ ->
        switch (proportionMoveBack >>> (identity   &&& isZero)) $ \_ ->
        switch (proportionWait     >>> (constant 0 &&& isOne))  $ \_ ->
        strangeClock dur wait

      where

        proportionMove     = time >>^ (/dur)               -- 0 to 1
        proportionMoveBack = time >>^ (dur -) >>^ (/dur)   -- 1 to 0
        proportionWait     = time >>^ (/wait)              -- 0 to 1
        isOne              = (>= 1) ^>> edge
        isZero             = (<= 0) ^>> edge

-- | Creates a block that slides sideways, waits for some time, slides back,
--   waits again, and repeats, with a vertical displacement of a sine.
sinusoidalBlock :: ObjectName
                -> Pos2D -> Size2D  -- Geometry
                -> Double           -- horizontal displacement
                -> Time             -- time to move
                -> Time             -- time to wait
                -> Double
                -> Time             -- time to move
                -> Time             -- t0
                -> AliveObject
sinusoidalBlock name (x0, y0) size hDisplacement moveDuration waitDuration vDisplacement verticalPeriod time0 =
  ListSF $ proc _ -> do

    -- Proportion is a number from 0 to 1. It increases for some time, stays at
    -- one, and goes back.
    propX <- strangeClock moveDuration waitDuration -< ()
    
    propY <- (sin . timeToPeriod . (+time0)) ^<< time -< ()
  
    -- Calculate position using the time-based proportion
    let x = x0 + propX * hDisplacement
        y = y0 + propY * vDisplacement
        p = (x, y)
  
    returnA -< (Object { objectName           = name
                       , objectKind           = Block
                       , objectProperties     = BlockProps size
                       , objectPos            = p
                       , objectVel            = (0,0)
                       , canCauseCollisions   = True
                       , collisionEnergy      = 0
                       }, False, [])
  
  where

    timeToPeriod :: Time -> Double
    timeToPeriod t = 2 * pi * t / verticalPeriod

    -- | Signal that goes from zero to one in 'dur' seconds, stays at one
    --   for 'wait' seconds, repeats in the opposite direction.
    strangeClock :: Time -> Time -> SF () Time
    strangeClock dur wait =
        switch (proportionMove     >>> (identity   &&& isOne))  $ \_ ->
        switch (proportionWait     >>> (constant 1 &&& isOne))  $ \_ ->
        switch (proportionMoveBack >>> (identity   &&& isZero)) $ \_ ->
        switch (proportionWait     >>> (constant 0 &&& isOne))  $ \_ ->
        strangeClock dur wait

      where

        proportionMove     = time >>^ (/dur)               -- 0 to 1
        proportionMoveBack = time >>^ (dur -) >>^ (/dur)   -- 1 to 0
        proportionWait     = time >>^ (/wait)              -- 0 to 1
        isOne              = (>= 1) ^>> edge
        isZero             = (<= 0) ^>> edge

-- | Creates a block that slides sideways, waits for some time, slides back,
--   waits again, and repeats, with a vertical displacement always down, by a
--   given amount after every step.
fallingBlock :: ObjectName
             -> Pos2D -> Size2D  -- Geometry
             -> Double           -- horizontal displacement
             -> Time             -- time to move
             -> Time             -- time to wait
             -> Double           -- displacement at the end
             -> Double           -- min vertical position
             -> AliveObject
fallingBlock name (x0, y0) size hDisplacement moveDuration waitDuration vDisplacement verticalMin =
  ListSF $ proc _ -> do

    -- Proportion is a number from 0 to 1. It increases for some time, stays at
    -- one, and goes back.
    propX <- strangeClock moveDuration waitDuration -< ()
    
    propY <- vStepClock -< ()
  
    -- Calculate position using the time-based proportion
    let x = x0 + propX * hDisplacement
        y = max verticalMin (y0 + propY * vDisplacement)
        p = (x, y)
  
    returnA -< (Object { objectName           = name
                       , objectKind           = Block
                       , objectProperties     = BlockProps size
                       , objectPos            = p
                       , objectVel            = (0,0)
                       , canCauseCollisions   = True
                       , collisionEnergy      = 0
                       }, False, [])
  
  where

    vStepClock :: SF () Double
    vStepClock = vStepClock' 0

    vStepClock' :: Double -> SF () Double
    vStepClock' n = switch (constant n &&& (time >>> (>= moveDuration) ^>> traceSF >>> edge >>^ (`tag` n)))
                           (\n' -> switch ((time >>^ (/waitDuration)) >>> (arr (+n') &&& ((>= 1) ^>> edge)))
                                          (\_ -> vStepClock' (n' + 1)))

    -- | Signal that goes from zero to one in 'dur' seconds, stays at one
    --   for 'wait' seconds, repeats in the opposite direction.
    strangeClock :: Time -> Time -> SF () Time
    strangeClock dur wait =
        switch (proportionMove     >>> (identity   &&& isOne))  $ \_ ->
        switch (proportionWait     >>> (constant 1 &&& isOne))  $ \_ ->
        switch (proportionMoveBack >>> (identity   &&& isZero)) $ \_ ->
        switch (proportionWait     >>> (constant 0 &&& isOne))  $ \_ ->
        strangeClock dur wait

      where

        proportionMove     = time >>^ (/dur)               -- 0 to 1
        proportionMoveBack = time >>^ (dur -) >>^ (/dur)   -- 1 to 0
        proportionWait     = time >>^ (/wait)              -- 0 to 1
        isOne              = (>= 1) ^>> edge
        isZero             = (<= 0) ^>> edge

-- | Static block builder, given a name, a size and its base
-- position.
disappearingBlock :: ObjectName -> Pos2D -> Size2D -> Time -> AliveObject
disappearingBlock name pos size maxTime = ListSF $ proc _ -> do
  t    <- time -< ()
  let dead = t >= maxTime
  returnA -< (Object { objectName           = name
                     , objectKind           = Block
                     , objectProperties     = BlockProps size
                     , objectPos            = pos
                     , objectVel            = (0,0)
                     , canCauseCollisions   = False
                     , collisionEnergy      = 0
                     }, dead, [])
