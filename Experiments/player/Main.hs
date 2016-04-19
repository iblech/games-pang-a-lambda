{-# LANGUAGE Arrows #-}
import Control.Monad
import Data.IORef
import Data.Maybe
import Debug.Trace
import FRP.Yampa                  as Yampa
import FRP.Yampa.Switches         as Yampa
import Graphics.UI.SDL            as SDL
import Graphics.UI.SDL.Primitives as SDL

width  = 640
height = 480

main = do
  timeRef <- newIORef (0 :: Int)
  controllerRef <- newIORef defaultController
  reactimate (initGraphs >> readIORef controllerRef)
             (\_ -> do
                dtSecs <- yampaSDLTimeSense timeRef
                mInput <- sdlGetController controllerRef
                -- print (mInput)
                return (dtSecs, Just mInput)
             )
             (\_ e -> display (e) >> return False)
             (dlSwitch [player] >>> arr composeWorld)

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

-- We need a non-blocking controller-polling function.
sdlGetController :: IORef Controller -> IO Controller
sdlGetController controllerState = do
  state <- readIORef controllerState
  e    <- pollEvent
  case e of

    -- Update position
    MouseMotion x y _ _ -> do
      writeIORef controllerState (state { controllerPos = (fromIntegral x, fromIntegral y)})
      sdlGetController controllerState

    -- Fire 1
    MouseButtonDown x y ButtonLeft -> do
      writeIORef controllerState (state { controllerFire1 = True })
      sdlGetController controllerState

    MouseButtonUp x y ButtonLeft -> do
      writeIORef controllerState (state { controllerFire1 = False })
      sdlGetController controllerState

    MouseButtonDown x y ButtonRight -> do
      writeIORef controllerState (state { controllerFire2 = True })
      sdlGetController controllerState

    MouseButtonUp x y ButtonRight -> do
      writeIORef controllerState (state { controllerFire2 = False })
      sdlGetController controllerState

    _                   -> return state

data Controller = Controller
 { controllerPos   :: (Double, Double)
 , controllerFire1 :: Bool
 , controllerFire2 :: Bool
 }

defaultController :: Controller
defaultController = Controller (0, 0) False False

initGraphs :: IO ()
initGraphs = do
  -- Initialise SDL
  SDL.init [InitVideo]

  -- Create window
  screen <- SDL.setVideoMode width height 16 [SWSurface]
  SDL.setCaption "Test" ""

  -- Important if we want the keyboard to work right (I don't know
  -- how to make it work otherwise)
  SDL.enableUnicode True

display :: World -> IO()
display world = do
  -- Obtain surface
  screen <- getVideoSurface

  -- Necessary colors
  let format = surfaceGetPixelFormat screen
  red      <- mapRGB format  0xFF 0    0
  green    <- mapRGB format  0x87 0xFF 0x87
  blue     <- mapRGB format  0    0    0xFF
  otherRed <- mapRGBA format 0xFF 0xFF 0x00 0xFF

  -- Paint screen green
  fillRect screen Nothing green

  -- Paint small red square
  case worldPlayer world of
    Just (Player (playerX, playerY)) -> void $ do
      let side = 10
          x = round playerX
          y = round playerY
      fillRect screen (Just (Rect x y side side)) red
    _ -> return ()

  -- Paint vertical lines for all fire arrows
  let paintFire f = do
        let fireColor = if fireSticky f then blue else otherRed
            (x0,y0)   = fireOrigin f
            (x1,y1)   = fireTip    f
            (dx, dy)  = (3, y0-y1)
            (x1', y1') = (round x1, round y1)
            (x0', y0', dx', dy') = (round x0 - 1, round y0, round dx, round dy)
        fillRect screen (Just (Rect x1' y1' dx' dy')) fireColor
        print f
        -- vLine screen x0'     y0' y1' fireColor
        -- vLine screen (x0'+1) y0' y1' fireColor
        -- vLine screen (x0'+2) y0' y1' fireColor

  mapM_ paintFire (worldFires world)

  -- Double buffering
  SDL.flip screen

data World = World
  { worldPlayer :: Maybe Player
  , worldFires  :: [Fire]
  }

composeWorld :: [Object] -> World
composeWorld objs = World pl fs
 where pl = listToMaybe $ catMaybes $ map objectPlayer objs
       fs = catMaybes $ map objectFire objs

objectPlayer :: Object -> Maybe Player
objectPlayer (ObjectPlayer pl) = Just pl
objectPlayer _                 = Nothing

objectFire :: Object -> Maybe Fire
objectFire (ObjectFire pl) = Just pl
objectFire _               = Nothing

data Object = ObjectPlayer Player
            | ObjectFire   Fire
 deriving Show

data Player = Player { playerPos :: (Double, Double) }
 deriving Show

data Fire = Fire   { fireOrigin :: (Double, Double)
                   , fireTip    :: (Double, Double)
                   , fireSticky :: Bool
                   }
 deriving Show

-- | A player that may die or spawn new objects.
player :: ListSF Controller Object
player = ListSF $ proc (Controller p f1 f2) -> do

  let this = ObjectPlayer $ Player p
  newF1 <- isEvent ^<< edge -< f1
  newF2 <- isEvent ^<< edge -< f2

  let newF1Arrows = [ fire p False | newF1 ]
      newF2Arrows = [ fire p True  | newF2 ]
      allArrows   = newF1Arrows ++ newF2Arrows

  returnA -< (this, False, allArrows)

 where initialPos = ((fromIntegral width/2), (fromIntegral height/2))

-- | This produces bullets that die when they hit the top of the screen.
-- There's sticky bullets and normal bullets. Sticky bullets get stuck for a
-- while before they die.
fire :: (Double, Double) -> Bool -> ListSF Controller Object
fire (x0, y0) sticky = ListSF $ proc _ -> do
  let v0 = (-10)

  -- Calculate tip of arrow
  yT <- (y0+) ^<< integral -< v0
  let y = max 0 yT

  -- Delay death if the fire is "sticky"
  hit <- switch (constant noEvent &&& (arr (<= 0) >>> edge))
                (\_ -> stickyDeath sticky)
      -< y
  let dead = isEvent hit

  let object = ObjectFire $ Fire (x0, y0) (x0, y) sticky

  returnA -< (object, dead, [])

stickyDeath True  = after 30 ()
stickyDeath False = constant (Event ())
