module Constants where

import Data.Word
import Graphics.UI.SDL as SDL

gameName :: String
gameName = "Break-a-ball"

width :: Double
width  = 1024
height :: Double
height = 600

gameWidth :: Double
gameWidth = width

gameHeight :: Double
gameHeight = height

-- Energy transmission between objects in collisions
velTrans :: Double
velTrans = 1.00

-- Max speed
maxVNorm :: Double -> Double
maxVNorm n
 | n > 15    = 800
 | n > 10    = 600
 | otherwise = 400

ballWidth, ballHeight :: Double
ballWidth  = 100
ballHeight = 100

ballMargin :: Double
ballMargin = 3

ballSize :: Integral a => a
ballSize = 25

-- Colors
fontColor :: SDL.Color
fontColor = SDL.Color 228 228 228

ballColor :: Word32
ballColor = 0xCC0011FF

velColor  :: Word32
velColor  = 0xCCBBFFFF

playerWidth :: Double
playerWidth = 30

playerHeight :: Double
playerHeight = 80

playerRightColor :: Word32
playerRightColor = 0xAA00AAFF

playerLeftColor :: Word32
playerLeftColor = 0x00AAAAFF

playerStandColor :: Word32
playerStandColor = 0xAAAA00FF

blockColor :: Word32
blockColor = 0xCAAC3DFF

playerSpeed :: Double
playerSpeed = 200

fireSpeed :: Double
fireSpeed = 200
