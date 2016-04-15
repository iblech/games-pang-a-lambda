{-# LANGUAGE Arrows #-}
import Data.IORef
import Data.List
import Debug.Trace
import FRP.Yampa       as Yampa
import FRP.Yampa.Switches as Yampa
import Graphics.UI.SDL as SDL

width  = 640
height = 480

main = do
  timeRef <- newIORef (0 :: Int)
  reactimate initGraphs
             (\_ -> do
                dtSecs <- yampaSDLTimeSense timeRef
                return (dtSecs, Nothing))
             (\_ e -> display e >> return False)
             mainSF

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
  let dtSecs = fromIntegral dt / 1000
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

  SDL.delay 10

-- | Main animation
mainSF :: SF () [(Double, Double)]
mainSF = dlSwitch initialList

-- | A list of position-producing forking-dying SFs
initialList :: [ListSF () (Double, Double)]
initialList = [ inCircles (320, 240) ]

-- | Describe a movement in circles, forking every few samples.
inCircles :: (Double, Double) -> ListSF () (Double, Double)
inCircles (baseX, baseY) = ListSF $ proc () -> do
   t <- localTime -< ()
   let radius = 30
       x = baseX + (cos t * radius)
       y = baseY + (sin t * radius)

   -- OffSpring
   split <- isEvent ^<< spike 300 -< ()
   let offspring = if split
                     then [inCircles (y*2, x/2), inCircles (x/2, y*2)]
                     else []

   -- Time to die?
   die   <- isEvent ^<< spike 350 -< ()

   returnA -< ((x,y), die, offspring)

   -- () <- arr id -< trace ( "Time : " ++ show t
   --                       ++ " mod: " ++ show (round t `mod` 10)
   --                       ++ " s: "   ++ show split
   --                       )
   --                       ()

-- | Produce a spike every few samples.
spike :: Int -> SF () (Yampa.Event ())
spike m = spikeBool m >>> edge
 where spikeBool m = resetCounter m >>> arr (== 0)

-- | Create a decreasing counter that is reset to the starting value when it
-- reaches 0.
resetCounter :: Int -> SF () Int
resetCounter m = loopPre m $ arr (snd >>> decR m >>> dup)
 where decR m n = if n == 0 then m else (n-1)
       dup  x   = (x,x)

-- Version of list splitting that works in traditional Yampa
-- inCirclesL' ips = inCirclesL ips >>> arr (map fst)
--
-- inCirclesL :: [SF () ((Double, Double), Bool)]
--            -> SF () [((Double, Double), Bool)]
-- inCirclesL ips = dpSwitchB ips evProd addToList
--
--  where
--      evProd :: SF ((), [((Double, Double), Bool)]) (Yampa.Event [(Double, Double)])
--      evProd = noEvent --> arr (snd >>> splitBalls)
--
-- initialList = [ inCircles (320, 240) ]
-- initialList' = [ inCircles' (320, 240) ]
--
-- addToList :: [SF () ((Double, Double), Bool)]
--           -> [(Double, Double)]
--           -> SF () [((Double, Double), Bool)]
-- addToList sfs ips = trace ("Adding new circles: " ++ show ips)
--                   $ inCirclesL (sfs ++ map inCircles ips)
--
-- splitBalls :: [((Double, Double), Bool)] -> Yampa.Event [(Double, Double)]
-- splitBalls ps
--   | null ls   = noEvent
--   | otherwise = Yampa.Event ls
--  where ls = [ (y-20, x+20) | ((x,y),True) <- ps ]
--
-- inCircles :: (Double, Double) -> SF () ((Double, Double), Bool)
-- inCircles (baseX, baseY) = proc () -> do
--    t <- localTime -< ()
--    let radius = 30
--        x = baseX + (cos t * radius)
--        y = baseY + (sin t * radius)
--
--    split <- noEvent --> spike 100 -< ()
--
--    () <- arr id -< trace ( "Time : " ++ show t
--                          ++ " mod: " ++ show (round t `mod` 10)
--                          ++ " s: "   ++ show split
--                          )
--                          ()
--
--    returnA -< ((x,y), isEvent split)
