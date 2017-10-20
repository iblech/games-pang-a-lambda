module Game.Constants where

import Data.Word
import Graphics.UI.SDL as SDL

gameName :: String
gameName = "Break-a-ball"

width :: Double
width  = 1024
height :: Double
height = 512

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
  | n >= 100  = 800
  | n >= 50   = 733
  | n >= 25   = 666
  | otherwise = 600

ballWidth, ballHeight :: Double
ballWidth  = 100
ballHeight = 100

-- ** Enemy sizes
ballGiant  = ballWidth
ballBig    = ballGiant  / 2
ballMedium = ballBig    / 2
ballSmall  = ballMedium / 2

ballMargin :: Double
ballMargin = 3

ballSize :: Integral a => a
ballSize = 25

-- Colors
fontColor :: SDL.Color
fontColor = SDL.Color 94 86 91

ballColor :: Word32
ballColor = 0xDD875FFF

velColor  :: Word32
velColor  = 0xCCBBFFFF

playerWidth :: Double
playerWidth = 65

playerHeight :: Double
playerHeight = 92

bulletColor :: Word32
bulletColor = 0xFFDDC34F

playerRightColor :: Word32
playerRightColor = 0xFFD2D454

playerLeftColor :: Word32
playerLeftColor = 0xFFB2D454

playerStandColor :: Word32
playerStandColor = 0xFFE5D454

playerBlinkRightColor :: Word32
playerBlinkRightColor = 0x88D2D454

playerBlinkLeftColor :: Word32
playerBlinkLeftColor = 0x88B2D454

playerBlinkStandColor :: Word32
playerBlinkStandColor = 0x88F2D454

backgroundColor :: Word32
backgroundColor = 0x88EDE9CB

blockColor :: Word32
blockColor = 0xFF85AABC

playerSpeed :: Double
playerSpeed = 200

bulletSpeed :: Double
bulletSpeed = 400

initialLives :: Int
initialLives = 5

-- Debugging collisions

collisionDebugColor :: Word32
collisionDebugColor = 0x88D2D4FF

collisionDebugThickness :: Num a => a
collisionDebugThickness = 3

debugCollisions :: Bool
debugCollisions = False
