{-# LANGUAGE Arrows     #-}
{-# LANGUAGE MultiWayIf #-}
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
import Prelude hiding (id, (.))
import FRP.Yampa
import FRP.Yampa.Extra
import FRP.Yampa.Switches

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
                     , canCauseCollisions   = False
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

-- | Moving block with an initial position and size, and horizontal and
-- vertical amplitude and periods. If an amplitude is /not/ zero, the block
-- moves along that dimension using a periodic oscillator (see 'osci').
arcBlock :: ObjectName
         -> Pos2D -> Size2D  -- Geometry
         -> Double -> Double -- Horizontal oscillation amplitude and period
         -> Double -> Double -- Vertical   oscillation amplitude and period
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
                     , canCauseCollisions   = False
                     , collisionEnergy      = 0
                     }, False, [])
