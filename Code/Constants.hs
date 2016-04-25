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
fontColor = SDL.Color 94 86 91

ballColor :: Word32
ballColor = 0xDD875FFF

velColor  :: Word32
velColor  = 0xCCBBFFFF

playerWidth :: Double
playerWidth = 30

playerHeight :: Double
playerHeight = 80

fireColor :: Word32
fireColor = 0xFFDDC34F

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
backgroundColor = 0xFFEDE9CB

blockColor :: Word32
blockColor = 0xFF85AABC

playerSpeed :: Double
playerSpeed = 200

fireSpeed :: Double
fireSpeed = 200

initialLives :: Int
initialLives = 5
