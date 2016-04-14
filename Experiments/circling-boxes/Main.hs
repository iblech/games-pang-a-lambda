{-# LANGUAGE Arrows #-}
import Graphics.UI.SDL as SDL
import FRP.Yampa       as Yampa
import Data.IORef

width  = 640
height = 480

main = do
  timeRef <- newIORef (0 :: Int)
  reactimate initGraphs
             (\_ -> do
                dtSecs <- yampaSDLTimeSense timeRef
                return (dtSecs, Nothing))
             (\_ e -> display e >> return False)
             inCirclesL

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

display :: [(Double,Double)] -> IO()
display xs = do
  -- Obtain surface
  screen <- getVideoSurface

  -- Paint screen green
  let format = surfaceGetPixelFormat screen
  green <- mapRGB format 0 0xFF 0
  fillRect screen Nothing green

  -- Paint small red square, at an angle 'angle' with respect to the center
  red <- mapRGB format 0xFF 0 0
  let side = 10
  let paintSquare (x,y) =
        fillRect screen (Just (Rect (round x) (round y) side side)) red

  mapM_ paintSquare xs

  -- Double buffering
  SDL.flip screen


inCirclesL :: SF () [(Double, Double)]
inCirclesL = parB [ inCircles (100, 100)
                  , inCircles (200, 200)
                  ]

inCircles :: (Double, Double) -> SF () (Double, Double)
inCircles (baseX, baseY) = proc () -> do
   t <- (/5) ^<< localTime -< ()
   let radius = 30
       x = baseX + (cos t * radius)
       y = baseY + (sin t * radius)
   returnA -< (x,y)
