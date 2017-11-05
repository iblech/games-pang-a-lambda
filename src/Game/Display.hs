{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Game.Display
  ( module Game.Display
  , module Game.ResourceManager
  )
  where

import           Control.Monad
import           Control.Monad.IfElse
import           Data.Tuple.Extra
import           Graphics.UI.SDL                 as SDL
import qualified Graphics.UI.SDL.TTF             as TTF
import           Graphics.UI.Extra.SDLDrawing    as SDL
import           Graphics.UI.Extra.SDLPrimitives as SDL
import           Text.Printf
import           Physics.Shapes.BasicCirclesAABB

import Game.Constants
import Game.GameState
import Game.Objects
import Game.ResourceManager

-- * Display handling

initializeDisplay :: IO ()
initializeDisplay =
  SDL.init [InitEverything]

initGraphs :: ResourceManager -> IO ()
initGraphs _res = void $ do
  screen <- SDL.setVideoMode (round width) (round height) 32 [SWSurface]
  SDL.setCaption gameName ""

  -- Important if we want the keyboard to work right (I don't know
  -- how to make it work otherwise)
  SDL.enableUnicode True

  -- Hide mouse
  SDL.showCursor True

-- * Rendering and Sound

-- | Loads new resources, renders the game state using SDL, and adjusts music.
render :: ResourceManager -> GameState -> IO()
render resourceManager shownState = do
  -- resources <- loadNewResources resourceManager shownState
  audio   resourceManager shownState
  display resourceManager shownState

-- ** Audio

audio :: ResourceManager -> GameState -> IO()
audio resources shownState = do
  -- -- Start bg music if necessary
  -- playing <- musicPlaying
  -- unless playing $ awhen (bgMusic resources) playMusic

  -- Play object hits
  mapM_ (audioObject resources) $ gameObjects shownState

audioObject :: ResourceManager -> Object -> IO ()
audioObject resources object = return ()

-- ** Painting
display :: ResourceManager -> GameState -> IO()
display resources shownState = do
  screen <- getVideoSurface

  -- Clear BG
  let lvl = gameLevel (gameInfo shownState)
  bg <- getBackgroundImage resources lvl
  blitSurface bg Nothing screen Nothing

  mapM_ (paintObject screen resources (gameTime (gameInfo shownState))) (gameObjects shownState)

  -- when debugCollisions $
  --   mapM_ (paintShape  screen resources (gameTime (gameInfo shownState))) (gameObjects shownState)

  -- HUD
  paintInfo screen resources (gameInfo shownState) (gameObjects shownState)

  -- eg. "Paused", "Level 0", etc.
  paintMessage screen resources (gameInfo shownState)

  -- Double buffering
  SDL.flip screen

paintInfo :: Surface -> ResourceManager -> GameInfo -> Objects -> IO()
paintInfo screen resRef over objs = do
  msg <- printSolid resRef ("Time: " ++ printf "%.2f" (gameTime over))
  renderAlignRight screen msg (10,50)
  awhen (findPlayer objs) $ \p -> do
    msg <- printSolid resRef ("Energy: " ++ show (playerEnergy p))
    renderAlignRight screen msg (10,100)

-- Render Level message
paintMessage :: Surface -> ResourceManager -> GameInfo -> IO()
paintMessage screen resources info =
    awhen (msg info) $ \msg' -> do
      message <- printSolid resources msg'
      renderAlignCenter screen message
  where
    msg info = case gameStatus info of
                 GameLoading -> Just ("Level " ++ show (gameLevel info))
                 _           -> Nothing

paintObject :: Surface -> ResourceManager -> Double -> Object -> IO ()
paintObject screen resRef time object = do
  let (px,py)  = objectPos object
  case objectProperties object of

    SideProps {} -> return ()

    BallProps ballSize -> void $ do

      -- Position
      let (x, y) = both round (px - ballSize, gameHeight - py - ballSize)

      -- Image
      ballImage <- getBallImage resRef (round ballSize)

      blitSurface ballImage Nothing screen (Just (Rect x y (-1) (-1)))

    BlockProps sz@(w', h') -> void $ do

      -- Position
      let (x,y) = both round (px, gameHeight - py - h')

      -- Image
      let blockImage = if w' > h' then getHBlockImage else getVBlockImage
      blockImg <- blockImage resRef

      blitSurface blockImg Nothing screen (Just (Rect x y (-1) (-1)))

    PlayerProps state _ vulnerable energy -> do
      let blinkOn = vulnerable || even (round (time * 10))
      when blinkOn $ void $ do

        -- Position
        let (x,y) = both round (px, gameHeight - py - playerHeight)

        -- Image
        playerImage <- getPlayerImage resRef state vulnerable

        blitSurface playerImage Nothing screen (Just (Rect x y (-1) (-1)))

    ProjectileProps -> void $ do

        -- Position
        let (x0, y0) = both round (px - 5, height - py)
            (dx, dy) = both round (10, py)

        -- Image
        fillRect screen (Just (Rect x0 y0 dx dy)) (Pixel bulletColor)

-- * Auxiliary drawing functions
printSolid :: ResourceManager -> String -> IO Surface
printSolid resources msg = do
  font    <- getResFont resources
  message <- TTF.renderTextSolid font msg fontColor
  return message

-- * Paint debugging physics information

-- TODO: Move to sage, generalise for physical objects, pass
-- game screen size, etc, as arguments.
paintShape :: Surface -> ResourceManager -> Double -> Object -> IO ()
paintShape screen resources time object =
  paintShape' screen resources (objShape object)

paintShape' :: Surface -> ResourceManager -> Shape -> IO ()
paintShape' screen resources (Rectangle (px, py) (w,h)) = void $
    drawThickRectangle screen (Rect x1 y1 x2 y2) (Pixel collisionDebugColor) collisionDebugThickness
  where
    (x1, y1) = both round (px, gameHeight - py - h)
    (x2, y2) = both round (px + w, gameHeight - py)
paintShape' screen resources (Circle (px, py) rd) = void $ do
    drawThickCircle screen x y r (Pixel collisionDebugColor) collisionDebugThickness
  where
    (x, y) = both round (px, gameHeight - py)
    r      = round rd
paintShape' screen resources (SemiPlane _pos s) =
  case s of
    LeftSide   -> drawThickLine screen 0 0 0 h (Pixel collisionDebugColor) collisionDebugThickness
    RightSide  -> drawThickLine screen w 0 w h (Pixel collisionDebugColor) collisionDebugThickness
    TopSide    -> drawThickLine screen 0 0 w 0 (Pixel collisionDebugColor) collisionDebugThickness
    BottomSide -> drawThickLine screen 0 h w h (Pixel collisionDebugColor) collisionDebugThickness
  where
    (w,h) = both round (width, height)
