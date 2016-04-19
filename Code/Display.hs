module Display where

import           Control.Arrow              ((***))
import           Control.Monad
import           FRP.Yampa.VectorSpace
import           Graphics.UI.SDL            as SDL
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.TTF        as TTF
import           Graphics.UI.Extra.SDL      as SDL
import           Text.Printf

import Constants
import GameState
import Objects
import Resources

-- | Ad-hoc resource loading
-- This function is ad-hoc in two senses: first, because it
-- has the paths to the files hard-coded inside. And second,
-- because it loads the specific resources that are needed,
-- so it's not a general, parameterised, scalable solution.
--
loadResources :: IO Resources
loadResources = do
  -- Font initialization
  _ <- TTF.init

  -- Load the fonts we need
  let gameFont = "data/lacuna.ttf"
  font  <- TTF.openFont gameFont 32 -- 32: fixed size?

  -- Load the fonts we need
  let gameFont = "data/lacuna.ttf"
  font2  <- TTF.openFont gameFont 8 -- 32: fixed size?

  -- Return all resources (just the font)
  return $ Resources font font2

initializeDisplay :: IO ()
initializeDisplay =
   -- Initialise SDL
  SDL.init [InitEverything]

initGraphs :: Resources -> IO ()
initGraphs _res = do
  screen <- SDL.setVideoMode (round width) (round height) 32 [SWSurface]
  SDL.setCaption gameName ""

  -- Important if we want the keyboard to work right (I don't know
  -- how to make it work otherwise)
  SDL.enableUnicode True

  -- Hide mouse
  SDL.showCursor True

  return ()

render :: Resources -> GameState -> IO()
render resources shownState = do
  -- Obtain surface
  screen <- getVideoSurface

  let format = surfaceGetPixelFormat screen
  bgColor <- mapRGB format 0x37 0x16 0xB4
  fillRect screen Nothing bgColor

  mapM_ (paintObject screen resources) $ gameObjects shownState

  displayInfo screen resources (gameInfo shownState)

  -- Double buffering
  SDL.flip screen

-- * Painting functions
displayInfo :: Surface -> Resources -> GameInfo -> IO()
displayInfo screen resources over =
  printAlignRight screen resources
    ("Time: " ++ printf "%.3f" (gameTime over)) (10,50)

paintObject :: Surface -> Resources -> Object -> IO ()
paintObject screen resources object =
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

    (Player state) -> do
      let (px,py)  = (\(u,v) -> (u, gameHeight - v - playerHeight)) (objectPos object)
      let (x,y)    = (round *** round) (px,py)
          (vx,vy)  = objectVel object
          (x',y')  = (round *** round) ((px,py) ^+^ (0.1 *^ (vx, -vy)))
          (w,h)    = (round playerWidth, round playerHeight)
          playerColor = case state of
                          PlayerRight -> playerRightColor
                          PlayerLeft  -> playerLeftColor
                          PlayerStand -> playerStandColor

      fillRect screen (Just (Rect x y w h)) (Pixel playerColor)

      _ <- SDLP.line screen (fromIntegral x) (fromIntegral y) x' y' (SDL.Pixel velColor)

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
        let fireColor = Pixel playerRightColor
            (x0,y0)   = (\(x,y) -> (x, height - y)) $ objectPos object
            (dx, dy)  = (10, snd (objectPos object))
            (x0', y0', dx', dy') = (round x0, round y0, round dx, round dy)
        fillRect screen (Just (Rect x0' y0' dx' dy')) fireColor
        return ()

-- * Render text with alignment
printAlignRight :: Surface -> Resources -> String -> (Int, Int) -> IO ()
printAlignRight screen resources msg (x,y) = void $ do
  let font = resFont resources
  message <- TTF.renderTextSolid font msg fontColor
  renderAlignRight screen message (x,y)
