{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Game.ResourceManager where

import           Data.IORef
import           Data.List.Extra
import           Data.Maybe            (fromJust)
import           Graphics.UI.SDL       as SDL
import           Graphics.UI.SDL.Image as SDL
import qualified Graphics.UI.SDL.TTF   as TTF

import Game.Objects
import Game.Resources

type ResourceManager = IORef Resources

-- * Resource handling

-- | Ad-hoc resource loading
-- This function is ad-hoc in two senses: first, because it
-- has the paths to the files hard-coded inside. And second,
-- because it loads the specific resources that are needed,
-- so it's not a general, parameterised, scalable solution.
--
loadResources :: IO (ResourceManager)
loadResources = do
  -- Font initialization
  _ <- TTF.init

  -- Load the fonts we need
  font            <- TTF.openFont "data/lacuna.ttf" 32 -- fixed size?
  font2           <- TTF.openFont "data/lacuna.ttf" 8  -- fixed size?
  backImages      <- mapM SDL.load [ "data/back.png" ]
  hitImages       <- mapM SDL.load [ "data/hit0.png" ] -- , "data/hit2.png"]
  walkLeftImages  <- mapM SDL.load [ "data/left-p3_walk01.png"
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

  walkRightImages <- mapM SDL.load [ "data/p3_walk01.png"
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

  standingImages  <- mapM SDL.load ["data/standing.png"]

  ballImages100   <- mapM SDL.load ["data/ball1-200.png", "data/ball2-200.png"]
  ballImages50    <- mapM SDL.load ["data/ball1-100.png", "data/ball2-100.png"]
  ballImages25    <- mapM SDL.load ["data/ball1-50.png",  "data/ball2-50.png"]
  ballImages12    <- mapM SDL.load ["data/ball1-25.png",  "data/ball2-25.png"]
  hblockImg       <- SDL.load "data/hblock100.png"
  vblockImg       <- SDL.load "data/vblock57.png"

  backgrounds <- mapM SDL.load ["data/level0.png","data/level1.png","data/level2.png","data/level3.png"]

  -- Return all resources (just the font)
  newIORef $ Resources font font2 [ (PlayerVisualStand, (0, standingImages))
                                  , (PlayerVisualRight, (0, walkRightImages))
                                  , (PlayerVisualLeft,  (0, walkLeftImages))
                                  , (PlayerVisualShoot, (0, backImages))
                                  , (PlayerVisualHit,   (0, hitImages))
                                  ]
                                  [ (0, ballImages100)
                                  , (0, ballImages50)
                                  , (0, ballImages25)
                                  , (0, ballImages12)
                                  ]
                                  hblockImg
                                  vblockImg
                                  backgrounds

getBallImage :: ResourceManager -> Int -> IO Surface
getBallImage resRef size = do
  resources <- readIORef resRef
  let ballIndex = case size of
                    100 -> 0
                    50  -> 1
                    25  -> 2
                    _   -> 3
  let (n, imgs) = (ballImages resources) !! ballIndex
      n' = if (n `div` 10) >= (length imgs - 1) then 0 else n + 1
      newImages = updateL (ballImages resources) ballIndex (n', imgs)
  writeIORef resRef (resources { ballImages = newImages })
  return (imgs!!(n `div` 10))

getPlayerImage :: ResourceManager -> PlayerState -> Bool -> IO Surface
getPlayerImage resRef state vulnerable = do
    resources <- readIORef resRef
    let (n, imgs) = fromJust $ lookup visualState (playerImages resources)
        n' = if (n `div` 2) >= (length imgs - 1) then 0 else (n+1)
        updateN o@(s, (n, is)) = if s == visualState then (s, (n', is)) else o
        newImages = map updateN (playerImages resources)
    writeIORef resRef (resources { playerImages = newImages })
    return (imgs!!(n `div` 2))
  where
    visualState = case (state, vulnerable) of
      (PlayerShooting, True)  -> PlayerVisualShoot
      (PlayerRight,    True)  -> PlayerVisualRight
      (PlayerLeft ,    True)  -> PlayerVisualLeft
      (PlayerStand,    True)  -> PlayerVisualStand
      (PlayerShooting, False) -> PlayerVisualShoot
      (PlayerRight,    False) -> PlayerVisualRight
      (PlayerLeft ,    False) -> PlayerVisualLeft
      (PlayerStand,    False) -> PlayerVisualHit

getBackgroundImage :: ResourceManager -> Int -> IO Surface
getBackgroundImage resRef lvl = do
  let safeBg l bgs = if l >= length bgs then last bgs else bgs !! l
  bg <- (safeBg lvl . backgrounds) <$> readIORef resRef
  return bg

getResFont :: ResourceManager -> IO TTF.Font
getResFont resRef = resFont <$> readIORef resRef

getHBlockImage resRef = hblockImage <$> readIORef resRef
getVBlockImage resRef = vblockImage <$> readIORef resRef
