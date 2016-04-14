{-# LANGUAGE Arrows #-}
import Graphics.UI.SDL            as SDL
import Graphics.UI.SDL.Primitives as SDL
import FRP.Yampa                  as Yampa
import Data.IORef
import Debug.Trace

width  = 640
height = 480

main = do
  timeRef <- newIORef (0 :: Int)
  reactimate initGraphs
             (\_ -> do
                dtSecs <- yampaSDLTimeSense timeRef
                return (dtSecs, Nothing))
             (\_ e -> display e >> return False)
             (fire (fromIntegral height / 2) (-10))

-- | Updates the time in an IO Ref and returns the time difference
updateTime :: IORef Int -> Int -> IO Int
updateTime timeRef newTime = do
  previousTime <- readIORef timeRef
  writeIORef timeRef newTime
  return (newTime - previousTime)

yampaSDLTimeSense :: IORef Int -> IO Yampa.DTime
yampaSDLTimeSense timeRef = do
  -- Get time passed since SDL init
  newTime <- fmap fromIntegral SDL.getTicks

  -- Obtain time difference
  dt <- updateTime timeRef newTime
  let dtSecs = fromIntegral dt / 100
  return dtSecs

initGraphs :: IO ()
initGraphs = do
  -- Initialise SDL
  SDL.init [InitVideo]

  -- Create window
  screen <- setVideoMode width height 16 [SWSurface]
  setCaption "Test" ""

display :: (Double, Double) -> IO()
display (boxY0,boxY) = do
  -- Obtain surface
  screen <- getVideoSurface

  -- Paint screen green
  let format = surfaceGetPixelFormat screen
  green <- mapRGB format 0 0xFF 0
  fillRect screen Nothing green

  -- Paint small red square, at an angle 'angle' with respect to the center
  red <- mapRGB format 0xFF 0xFF 0
  let x  = fromIntegral $ width `div` 2
      y0 = round boxY0
      y  = round boxY
  vLine screen x y0 y red

  -- Double buffering
  SDL.flip screen

rising :: Double -> Double -> SF () (Double, Double)
rising y0 v0 = proc () -> do
  y <- (y0+) ^<< integral -< v0
  returnA -< (y0, y)

fire :: Double -> Double -> SF () (Double, Double)
fire y vy = switch (rising y vy >>> (Yampa.identity &&& hitCeiling))
                   (\(y0, yn) -> switch (constant (y0, yn) &&& after 10 ())
                                        (\_ -> fire y vy))

hitCeiling :: SF (Double, Double) (Yampa.Event (Double, Double))
hitCeiling = arr (\(y0,y) ->
                   let boxTop = y
                   in if boxTop < 0
                        then Yampa.Event (y0, y)
                        else Yampa.NoEvent)
