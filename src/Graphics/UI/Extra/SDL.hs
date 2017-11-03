module Graphics.UI.Extra.SDL where

import Control.Monad
import Data.IORef
import Graphics.UI.SDL
import qualified Graphics.UI.SDL.Primitives as SDLP

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
      tHeight = surfaceGetHeight screen
      w       = surfaceGetWidth  surface
      h       = surfaceGetHeight surface
      px      = (tWidth - w) `div` 2
      py      = (tHeight - h) `div` 2
      rect    = Rect px py w h
  blitSurface surface Nothing screen (Just rect)

drawThickRectangle surface (Rect x1 y1 x2 y2) pixel 0 = return ()
drawThickRectangle surface rect@(Rect x1 y1 x2 y2) pixel n = do
  let n' = n-1
  SDLP.rectangle surface (Rect (x1-n') (y1-n') (x2+n') (y2+n')) pixel
  drawThickRectangle surface rect pixel n'

drawThickCircle screen x y r pixel 0 = return ()
drawThickCircle screen x y r pixel n = do
  let n' = n-1
  SDLP.circle screen x y (r+n') pixel
  drawThickCircle screen x y r pixel n'

drawThickLine screen x1 y1 x2 y2 pixel 0 = return ()
drawThickLine screen x1 y1 x2 y2 pixel n = do
  let n' = n-1
  SDLP.line screen (x1) (y1-n') (x2) (y2-n') pixel
  SDLP.line screen (x1) (y1+n') (x2) (y2+n') pixel
  SDLP.line screen (x1-n') (y1) (x2-n') (y2) pixel
  SDLP.line screen (x1+n') (y1) (x2+n') (y2) pixel
  drawThickLine screen x1 y1 x2 y2 pixel n'
