{-# LANGUAGE Arrows     #-}
{-# LANGUAGE MultiWayIf #-}
-- | This module defines the time transformation functions.
module Game.Time where

-- External imports
import FRP.Yampa
import FRP.Yampa.Extra

-- Internal iports
import Game.Input
import Game.ObjectSF

-- | Time transformation that allows time to be reversed.
timeProgressionReverse :: SF Controller (DTime -> DTime)
timeProgressionReverse = proc (c) -> do
  -- NOTE: Another option is slowDown
  let rev  = if controllerReverse c then ((-1)*) else id
  returnA -< rev

-- | Time transformation that slows down time upon request.
timeProgressionSlowDown :: SF Controller (DTime -> DTime)
timeProgressionSlowDown = proc (c) -> do
  rec let slow = controllerReverse c
          unit = if | power' >= 0 && slow -> (-1)
                    | power' >= maxPower  -> 0
                    | otherwise           -> 1
      power <- (maxPower +) ^<< integral -< unit
      let power' = min maxPower (max 0 power)
          dtF    = if slow && (power' > 0) then (0.1*) else id
  returnA -< dtF
 where
   maxPower :: Double
   maxPower = 5

-- | Time transformation that can halt time for an object.
timeProgressionHalt :: SF ObjectInput (DTime -> DTime)
timeProgressionHalt =   constant id        &&& mustHalt
                    ||> constant (const 0) &&& after 25 ()
                    ||> timeProgressionHalt
 where
   mustHalt = (controllerHalt . userInput) ^>> edge
