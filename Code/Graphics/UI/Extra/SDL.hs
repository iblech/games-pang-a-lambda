module Graphics.UI.Extra.SDL where

import Control.Monad
import Data.IORef
import Graphics.UI.SDL

-- * Types

-- Auxiliary SDL stuff
isEmptyEvent :: Event -> Bool
isEmptyEvent NoEvent = True
isEmptyEvent _       = False

-- * SDL-based clock

initializeTimeRef :: IO (IORef Int)
initializeTimeRef = do
  -- Weird shit I have to do to get accurate time!
  timeRef <- newIORef (0 :: Int)
  _       <- senseTimeRef timeRef
  _       <- senseTimeRef timeRef
  _       <- senseTimeRef timeRef
  _       <- senseTimeRef timeRef

  return timeRef

senseTimeRef :: IORef Int -> IO Int
senseTimeRef timeRef = do
  -- Get time passed since SDL init
  newTime <- fmap fromIntegral getTicks

  -- Obtain time difference
  dt <- updateTime timeRef newTime
  return dt

-- | Updates the time in an IO Ref and returns the time difference
updateTime :: IORef Int -> Int -> IO Int
updateTime timeRef newTime = do
  previousTime <- readIORef timeRef
  writeIORef timeRef newTime
  return (newTime - previousTime)

milisecsToSecs :: Int -> Double
milisecsToSecs m = fromIntegral m / 1000

-- * Rendering

renderAlignRight :: Surface -> Surface -> (Int, Int) -> IO ()
renderAlignRight screen surface (x,y) = void $ do
  let rightMargin = surfaceGetWidth screen
      w           = surfaceGetWidth  surface
      h           = surfaceGetHeight surface
      rect        = Rect (rightMargin - x - w) y w h
  blitSurface surface Nothing screen (Just rect)

renderAlignCenter :: Surface -> Surface -> IO ()
renderAlignCenter screen surface = void $ do
  let tWidth  = surfaceGetWidth screen
      tHeight = surfaceGetWidth screen
      w       = surfaceGetWidth  surface
      h       = surfaceGetHeight surface
      px      = (tWidth - w) `div` 2
      py      = (tHeight - h) `div` 2
      rect    = Rect px py w h
  blitSurface surface Nothing screen (Just rect)
