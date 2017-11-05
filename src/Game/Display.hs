{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Game.Display
  ( module Game.Display
  , module Game.ResourceManager
  )
  where

import           Control.Monad
import           Control.Monad.IfElse
import           Data.Tuple.Extra
import           FRP.Yampa.VectorSpace
import           Graphics.UI.SDL                 as SDL
import qualified Graphics.UI.SDL.TTF             as TTF
import           Graphics.UI.Extra.SDLDrawing    as SDL
import           Graphics.UI.Extra.SDLPrimitives as SDL
import           Text.Printf
import           Physics.Shapes.BasicCirclesAABB

import Game.Constants
import Game.GameState
import Game.Objects
import Game.Resources
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

-- * Rendering

render :: ResourceManager -> GameState -> IO()
render resources shownState = do
  screen <- getVideoSurface

  -- Clear BG
  -- fillRect screen Nothing (Pixel backgroundColor)
  let lvl = gameLevel (gameInfo shownState)
  bg <- getBackgroundImage resources lvl
  blitSurface bg Nothing screen Nothing

  mapM_ (paintObject screen resources (gameTime (gameInfo shownState))) (gameObjects shownState)

  -- when debugCollisions $
  --   mapM_ (paintShape  screen resources (gameTime (gameInfo shownState))) (gameObjects shownState)

  -- HUD
  displayInfo screen resources (gameInfo shownState) (gameObjects shownState)

  -- eg. "Paused", "Level 0", etc.
  displayMessage screen resources (gameInfo shownState)

  -- Double buffering
  SDL.flip screen

-- ** Painting functions
displayInfo :: Surface -> ResourceManager -> GameInfo -> Objects -> IO()
displayInfo screen resRef over objs = do
  msg <- printSolid resRef ("Time: " ++ printf "%.2f" (gameTime over))
  renderAlignRight screen msg (10,50)
  awhen (findPlayer objs) $ \p -> do
    msg <- printSolid resRef ("Energy: " ++ show (playerEnergy p))
    renderAlignRight screen msg (10,100)

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

paintShape :: Surface -> ResourceManager -> Double -> Object -> IO ()
paintShape screen resources time object =
  paintShape' screen resources time (objShape object)

paintShape' :: Surface -> ResourceManager -> Double -> Shape -> IO ()
paintShape' screen resources time (Rectangle (px, py) (w,h)) = void $
    drawThickRectangle screen (Rect x1 y1 x2 y2) (Pixel collisionDebugColor) collisionDebugThickness
  where
    (x1, y1) = both round (px, gameHeight - py - h)
    (x2, y2) = both round (px + w, gameHeight - py)
paintShape' screen resources time (Circle (px, py) rd) = void $ do
    drawThickCircle screen x y r (Pixel collisionDebugColor) collisionDebugThickness
  where
    (x, y) = both round (px, gameHeight - py)
    r      = round rd
paintShape' screen resources time (SemiPlane (px, py) s) =
  case s of
    LeftSide   -> drawThickLine screen 0 0 0 h (Pixel collisionDebugColor) collisionDebugThickness
    RightSide  -> drawThickLine screen w 0 w h (Pixel collisionDebugColor) collisionDebugThickness
    TopSide    -> drawThickLine screen 0 0 w 0 (Pixel collisionDebugColor) collisionDebugThickness
    BottomSide -> drawThickLine screen 0 h w h (Pixel collisionDebugColor) collisionDebugThickness
  where
    (w,h) = both round (width, height)

-- | Render Level message
displayMessage :: Surface -> ResourceManager -> GameInfo -> IO()
displayMessage screen resources info = case gameStatus info of
  GameLoading -> do
    msg <- printSolid resources ("Level " ++ show (gameLevel info))
    renderAlignCenter screen msg
  _ -> return ()

-- * Auxiliary drawing functions
printSolid :: ResourceManager -> String -> IO Surface
printSolid resources msg = do
  font    <- getResFont resources
  message <- TTF.renderTextSolid font msg fontColor
  return message
