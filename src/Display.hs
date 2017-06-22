{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Display where

import           Control.Arrow              ((***))
import           Control.Monad
import           Data.IORef
import           Data.Maybe (fromJust)
import           FRP.Yampa.VectorSpace
import           Graphics.UI.SDL            as SDL
import           Graphics.UI.SDL.Image      as SDL
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.TTF        as TTF
import           Graphics.UI.Extra.SDL      as SDL
import           Text.Printf

import Constants
import GameState
import Objects
import Resources
import Physics.TwoDimensions.Shapes



-- | Ad-hoc resource loading
-- This function is ad-hoc in two senses: first, because it
-- has the paths to the files hard-coded inside. And second,
-- because it loads the specific resources that are needed,
-- so it's not a general, parameterised, scalable solution.
--
loadResources :: IO (IORef Resources)
loadResources = do
  -- Font initialization
  _ <- TTF.init

  -- Load the fonts we need
  let gameFont = "data/lacuna.ttf"
  font  <- TTF.openFont gameFont 32 -- 32: fixed size?

  -- Load the fonts we need
  let gameFont = "data/lacuna.ttf"
  font2  <- TTF.openFont gameFont 8 -- 32: fixed size?

  backImages <- mapM (SDL.load) ["data/back.png"]
  hitImages  <- mapM (SDL.load) ["data/hit1.png", "data/hit2.png"]
  walkLeftImages <- mapM (SDL.load) [ "data/left-p3_walk01.png"
                                    , "data/left-p3_walk02.png"
                                    , "data/left-p3_walk03.png"
                                    , "data/left-p3_walk04.png"
                                    , "data/left-p3_walk05.png"
                                    , "data/left-p3_walk06.png"
                                    , "data/left-p3_walk07.png"
                                    , "data/left-p3_walk08.png"
                                    , "data/left-p3_walk09.png"
                                    , "data/left-p3_walk10.png"
                                    , "data/left-p3_walk11.png"
                                    ]
  walkRightImages <- mapM (SDL.load) [ "data/p3_walk01.png"
                                     , "data/p3_walk02.png"
                                     , "data/p3_walk03.png"
                                     , "data/p3_walk04.png"
                                     , "data/p3_walk05.png"
                                     , "data/p3_walk06.png"
                                     , "data/p3_walk07.png"
                                     , "data/p3_walk08.png"
                                     , "data/p3_walk09.png"
                                     , "data/p3_walk10.png"
                                     , "data/p3_walk11.png"
                                     ]
  standingImages <- mapM (SDL.load) ["data/standing.png"]

  -- Return all resources (just the font)
  newIORef $ Resources font font2 standingImages walkRightImages walkLeftImages backImages hitImages

initializeDisplay :: IO ()
initializeDisplay =
   -- Initialise SDL
  SDL.init [InitEverything]

initGraphs :: IORef Resources -> IO ()
initGraphs _res = do
  screen <- SDL.setVideoMode (round width) (round height) 32 [SWSurface]
  SDL.setCaption gameName ""

  -- Important if we want the keyboard to work right (I don't know
  -- how to make it work otherwise)
  SDL.enableUnicode True

  -- Hide mouse
  SDL.showCursor True

  return ()

render :: IORef Resources -> GameState -> IO()
render resources shownState = do
  -- Obtain surface
  screen <- getVideoSurface

  -- Clear BG
  fillRect screen Nothing (Pixel backgroundColor)

  -- Paint objects
  mapM_ (paintObject screen resources (gameTime (gameInfo shownState))) (gameObjects shownState)

  when debugCollisions $
    mapM_ (paintShape  screen resources (gameTime (gameInfo shownState))) (gameObjects shownState)

  -- Paint HUD
  displayInfo screen resources (gameInfo shownState) (gameObjects shownState)

  -- Paint messages/popups (eg. "Paused", "Level 0", etc.)
  displayMessage screen resources (gameInfo shownState)

  -- Double buffering
  SDL.flip screen

-- * Painting functions
displayInfo :: Surface -> IORef Resources -> GameInfo -> Objects -> IO()
displayInfo screen resRef over objs = do
  printAlignRight screen resRef
    ("Time: " ++ printf "%.2f" (gameTime over)) (10,50)
  let p = findPlayer objs
  case p of
    Just p' -> let e = playerEnergy p'
               in printAlignRight screen resRef ("Energy: " ++ show e) (10,100)
    Nothing -> return ()

paintObject :: Surface -> IORef Resources -> Double -> Object -> IO ()
paintObject screen resRef time object = do
  resources <- readIORef resRef
  case objectKind object of
    (Side {}) -> return ()
    (Ball ballSize) -> do
      let (px,py)  = (\(u,v) -> (u, gameHeight - v)) (objectPos object)
      let (x,y)    = (round *** round) (px,py)
          (vx,vy)  = objectVel object
          (x',y')  = (round *** round) ((px,py) ^+^ (0.1 *^ (vx, -vy)))
      _ <- SDLP.filledCircle screen x y (round ballSize) (SDL.Pixel ballColor)
      _ <- SDLP.line screen x y x' y' (SDL.Pixel velColor)

      -- Print position
      let font = miniFont resources
      message <- TTF.renderTextSolid font (show $ (round *** round) (objectPos object)) fontColor
      let w           = SDL.surfaceGetWidth  message
          h           = SDL.surfaceGetHeight message
          (x'',y'')   = (round *** round) (px,py)
          rect        = SDL.Rect (x''+30) (y''-30) w h
      SDL.blitSurface message Nothing screen (Just rect)
      return ()

    (Block sz@(w', h')) -> void $ do
      let (px,py)  = (objectPos object)
          (x,y)    = (round *** round) (px,gameHeight - py -h')
          (w,h)    = (round *** round) sz
      fillRect screen (Just (Rect x y w h)) (Pixel blockColor)

    (Player state _ vulnerable energy) -> do
      let blinkOn  = vulnerable || (even (round (time * 10)))
      when blinkOn $ do

        let (px,py)  = (\(u,v) -> (u, gameHeight - v - playerHeight)) (objectPos object)
        let (x,y)    = (round *** round) (px,py)
            (vx,vy)  = objectVel object
            (x',y')  = (round *** round) ((px,py) ^+^ (0.1 *^ (vx, -vy)))
            (w,h)    = (round playerWidth, round playerHeight)
            playerColor = case (state, vulnerable) of
                            (PlayerShooting, True)  -> playerRightColor
                            (PlayerRight,    True)  -> playerRightColor
                            (PlayerLeft ,    True)  -> playerLeftColor
                            (PlayerStand,    True)  -> playerStandColor
                            (PlayerRight,    False) -> playerBlinkRightColor
                            (PlayerLeft ,    False) -> playerBlinkLeftColor
                            (PlayerStand,    False) -> playerBlinkStandColor
            playerImage = case (state, vulnerable) of
                            (PlayerShooting, True)  -> head (shootingImage resources) 
                            (PlayerRight,    True)  -> head (rightImage    resources) 
                            (PlayerLeft ,    True)  -> head (leftImage     resources) 
                            (PlayerStand,    True)  -> head (standingImage resources)
                            (PlayerShooting, False) -> head (shootingImage resources) 
                            (PlayerRight,    False) -> head (rightImage    resources) 
                            (PlayerLeft ,    False) -> head (leftImage     resources) 
                            (PlayerStand,    False) -> head (standingImage resources)

        fillRect screen (Just (Rect x y w h)) (Pixel playerColor)
        blitSurface playerImage Nothing screen (Just (Rect x y (-1) (-1)))

        -- _ <- SDLP.line screen (fromIntegral x) (fromIntegral y) x' y' (SDL.Pixel velColor)

        -- Print position
        let font = miniFont resources
        message <- TTF.renderTextSolid font (show $ (round *** round) (objectPos object)) fontColor
        let w           = SDL.surfaceGetWidth  message
            h           = SDL.surfaceGetHeight message
            (x'',y'')   = (round *** round) (px,py)
            rect        = SDL.Rect (x''+30) (y''-30) w h
        SDL.blitSurface message Nothing screen (Just rect)
        return ()

    Projectile -> do
        let (x0,y0)   = (\(x,y) -> (x - 5, height - y)) $ objectPos object
            (dx, dy)  = (10, snd (objectPos object))
            (x0', y0', dx', dy') = (round x0, round y0, round dx, round dy)
        fillRect screen (Just (Rect x0' y0' dx' dy')) (Pixel fireColor)
        return ()

paintShape :: Surface -> IORef Resources -> Double -> Object -> IO ()
paintShape screen resources time object =
 paintShape' screen resources time (objShape object)

paintShape' screen resources time shape =
  case shape of
    Rectangle (px, py) (w,h) -> void $ do
      let x1 = round px
          x2 = round (px + w)
          y1 = round (gameHeight - py - h)
          y2 = round (gameHeight - py)
      drawThickRectangle screen (Rect x1 y1 x2 y2) (Pixel collisionDebugColor) collisionDebugThickness

    Circle    (px, py) rd -> void $ do
      let x = round px
          y = round (gameHeight - py)
          r = round rd
      drawThickCircle screen x y r (Pixel collisionDebugColor) collisionDebugThickness

    SemiPlane (px, py) s ->
      let w = round width
          h = round height
      in case s of
           LeftSide   -> drawThickLine screen 0 0 0 h (Pixel collisionDebugColor) collisionDebugThickness
           RightSide  -> drawThickLine screen w 0 w h (Pixel collisionDebugColor) collisionDebugThickness
           TopSide    -> drawThickLine screen 0 0 w 0 (Pixel collisionDebugColor) collisionDebugThickness
           BottomSide -> drawThickLine screen 0 h w h (Pixel collisionDebugColor) collisionDebugThickness

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

-- * Painting functions
displayMessage :: Surface -> IORef Resources -> GameInfo -> IO()
displayMessage screen resources info = case gameStatus info of
  GameLoading ->
    printAlignCenter screen resources ("Level " ++ show (gameLevel info))
  _ -> return ()

-- * Render text with alignment
printAlignRight :: Surface -> IORef Resources -> String -> (Int, Int) -> IO ()
printAlignRight screen resources msg (x,y) = void $ do
  font <- resFont <$> readIORef resources
  message <- TTF.renderTextSolid font msg fontColor
  renderAlignRight screen message (x,y)

-- * Render text with alignment
printAlignCenter :: Surface -> IORef Resources -> String -> IO ()
printAlignCenter screen resources msg = void $ do
  font <- resFont <$> readIORef resources
  message <- TTF.renderTextSolid font msg fontColor
  renderAlignCenter screen message
