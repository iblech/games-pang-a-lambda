-- | This module defines the levels. 
--
-- Objects are represented as Signal Functions ('ObjectSF'). In this
-- game we are introducing a novel construct, named 'AliveObject' in this
-- a particular instance of a 'ListSF'. A 'ListSF' is a signal function that
-- can die, or can produce more signal functions of the same type.
-- 
-- Each object is responsible for itself, but it cannot affect others:
-- objects can watch others, depend on others and react to them, but they
-- cannot /send a message/ or eliminate other objects. 
--
-- This module contains two sections:
--
--   - A collection of gameplay SFs, which control the core game loop, carry
--   out collision detection, etc.
--
--   - SF for each game object. These define the elements in the game universe,
--   which can observe other elements, depend on user input, on previous
--   collisions, etc.
--
-- You may want to read the basic definition of 'GameState', 'Controller' and
-- 'ObjectSF' before you attempt to go through this module.
--
module Game.Levels where

-- External imports
import FRP.Yampa.Extra

-- Internal iports
import Game.Constants
import Game.ObjectSF
import Game.Objects.Walls
import Game.Objects.Player
import Game.Objects.Balls
import Game.Objects.Blocks

-- | Player for each level.
objPlayers :: Int -> [AliveObject]
objPlayers _ =
  [ player "player" initialLives (320, 20) True ]

-- | Enemies depending on the level.

-- WARNING: All objects need different names, both at the beginning and during
-- gameplay.
objEnemies :: Int -> [AliveObject]
objEnemies 0 =
  [ splittingBall "ballEnemy1" ballWidth (600, 300) (360, -350) ]
objEnemies 1 =
  [ splittingBall "ballEnemy1" ballMedium (width/4, 300)   (360, -350)
  , splittingBall "ballEnemy2" ballMedium (3*width/4, 300) (360, -350) ]
objEnemies 2 =
  map ballLeft [1..4] ++ map ballRight [1..4]
 where baseL = 20
       sep   = width / 20
       baseR = width - (baseL  + 4 * sep)

       ballLeft n = splittingBall ("ballEnemyL" ++ show n)
                         ballSmall (baseL + n * sep, 100) (-200, -200)

       ballRight n = splittingBall ("ballEnemyR" ++ show n)
                         ballSmall (baseR + n * sep, 100) (200, -200)
objEnemies _ =
  [ splittingBall "ballEnemy1" ballBig (600, 300) (360, -350) ]

-- Blocks are horizontal rectangles that /every/ other element collides
-- with. They need not be static.

-- | List of blocks depending on the level.
objBlocks :: Int -> [AliveObject]
objBlocks 0 = [ staticBlock      "block1" (200, 55)  (100, 57)               ]
objBlocks 1 = [ oscillatingBlock "block1" (400, 200) (100, 57) 200 10   0  0 ]
objBlocks 2 = [ oscillatingBlock "block1" (400, 200) (100, 57) 0    0 100 10 ]
objBlocks 3 = [ oscillatingBlock "block1" (324, 200) (100, 57) 200  6   0  0
              , oscillatingBlock "block2" (700, 200) (100, 57) 200  6 100 10
              ]
objBlocks 4 = [ staticBlock      "block11" (100, 125) (100, 57)
              , staticBlock      "block12" (100, 200) (100, 57)
              , staticBlock      "block13" (100, 275) (100, 57)
              , staticBlock      "block14" (100, 350) (100, 57)
              , staticBlock      "block15" (100, 425) (100, 57)
              , staticBlock      "block21" (350, 125) (100, 57)
              , staticBlock      "block22" (350, 200) (100, 57)
              , staticBlock      "block23" (350, 275) (100, 57)
              , staticBlock      "block24" (350, 350) (100, 57)
              , staticBlock      "block25" (350, 425) (100, 57)
              , staticBlock      "block31" (600, 125) (100, 57)
              , staticBlock      "block32" (600, 200) (100, 57)
              , staticBlock      "block33" (600, 275) (100, 57)
              , staticBlock      "block34" (600, 350) (100, 57)
              , staticBlock      "block35" (600, 425) (100, 57)
              , staticBlock      "block41" (850, 125) (100, 57)
              , staticBlock      "block42" (850, 200) (100, 57)
              , staticBlock      "block43" (850, 275) (100, 57)
              , staticBlock      "block44" (850, 350) (100, 57)
              , staticBlock      "block45" (850, 425) (100, 57)
              ]
objBlocks 5 = [ oscillatingBlock "block1" (100, 200) (100, 57) 100 10 100 10
              , oscillatingBlock "block2" (350, 200) (100, 57) 100 10 100 10
              , oscillatingBlock "block3" (600, 200) (100, 57) 100 10 100 10
              , oscillatingBlock "block4" (850, 200) (100, 57) 100 10 100 10
              ]
objBlocks 6 = [ oscillatingBlock "block1" (100, 200) (100, 57) 100 10 100 10
              , oscillatingBlock "block2" (350, 400) (100, 57) (-100) 5 100 5
              , oscillatingBlock "block3" (600, 200) (100, 57) 100 10 100 10
              , oscillatingBlock "block4" (850, 400) (100, 57) (-100) 5 100 5
              ]
objBlocks 7 = [ arcBlock "block1" (100, 200) (100, 57) 100 10    100 10
              , arcBlock "block2" (350, 200) (100, 57) 100 (-10) 100 (-10)
              , arcBlock "block3" (600, 200) (100, 57) 100 10    100 10
              , arcBlock "block4" (850, 200) (100, 57) 100 (-10) 100 (-10)
              ]
objBlocks 8 = [ slidingBlock "block11" (100, 200) (100, 57) 100    5 2
              , slidingBlock "block12" (350, 200) (100, 57) 100    5 2
              , slidingBlock "block13" (600, 200) (100, 57) 100    5 2
              , slidingBlock "block14" (850, 200) (100, 57) 100    5 2
              , slidingBlock "block21" (200, 300) (100, 57) (-100) 5 2
              , slidingBlock "block22" (450, 300) (100, 57) (-100) 5 2
              , slidingBlock "block23" (700, 300) (100, 57) (-100) 5 2
              , slidingBlock "block24" (950, 300) (100, 57) (-100) 5 2
              , slidingBlock "block31" (100, 400) (100, 57) 100    5 2
              , slidingBlock "block32" (350, 400) (100, 57) 100    5 2
              , slidingBlock "block33" (600, 400) (100, 57) 100    5 2
              , slidingBlock "block34" (850, 400) (100, 57) 100    5 2
              ]
objBlocks 9  = [ sinusoidalBlock "block1"  (100, 300) (100, 57) 800 10 0 100 2 0]
objBlocks 10 = [ staticBlock "block1" (100, 200)             (100, 57) 
               , staticBlock "block2" (200, 200)             (100, 57) 
               , staticBlock "block3" (100, 400)             (100, 57) 
               , staticBlock "block4" (gameWidth - 100, 200) (100, 57) 
               , staticBlock "block5" (gameWidth - 200, 200) (100, 57) 
               , staticBlock "block6" (gameWidth - 200, 400) (100, 57) 
               ]
objBlocks 11 = [ oscillatingBlock "block1" (100, 200)             (100, 57) 0 0 100 10
               , oscillatingBlock "block2" (200, 200)             (100, 57) 0 0 100 10
               , staticBlock      "block3" (100, 400)             (100, 57) 
               , oscillatingBlock "block4" (gameWidth - 100, 200) (100, 57) 0 0 100 10
               , oscillatingBlock "block5" (gameWidth - 200, 200) (100, 57) 0 0 100 10
               , staticBlock      "block6" (gameWidth - 200, 400) (100, 57) 
               ]
objBlocks 12 = [ oscillatingBlock "block1" (100, 200)             (100, 57) 0 0 100    10
               , oscillatingBlock "block2" (200, 200)             (100, 57) 0 0 (-100) 10
               , staticBlock      "block3" (100, 400)             (100, 57) 
               , oscillatingBlock "block4" (gameWidth - 100, 200) (100, 57) 0 0 100    10
               , oscillatingBlock "block5" (gameWidth - 200, 200) (100, 57) 0 0 (-100) 10
               , staticBlock      "block6" (gameWidth - 200, 400) (100, 57) 
               ]
objBlocks 13 = [ staticBlock "block1" (100, 200)             (100, 57)
               , staticBlock "block2" (200, 200)             (100, 57)
               , staticBlock "block3" (300, 200)             (100, 57) 
               , staticBlock "block4" (gameWidth - 100, 200) (100, 57)
               , staticBlock "block5" (gameWidth - 200, 200) (100, 57)
               , staticBlock "block6" (gameWidth - 300, 200) (100, 57) 
               ]

objBlocks 14 = [ fallingBlock "block1" (0, gameHeight - 100) (100, 57) (gameWidth - 100) 10 2 (-80) 110]

objBlocks 15 = [ staticBlock "block1" (gameWidth / 2 - 100, 110) (100, 57)
               , staticBlock "block2" (gameWidth / 2 - 200, 110) (100, 57)
               , staticBlock "block3" (gameWidth / 2 - 300, 110) (100, 57) 
               , staticBlock "block6" (gameWidth / 2      , 110) (100, 57) 
               , staticBlock "block4" (gameWidth / 2 + 100, 110) (100, 57)
               , staticBlock "block5" (gameWidth / 2 + 200, 110) (100, 57)
               ]

objBlocks 16 = [ staticBlock "block2" (gameWidth / 2 - 200, 200) (100, 57)
               , staticBlock "block5" (gameWidth / 2 + 200, 200) (100, 57)
               ]

objBlocks 17 = [ oscillatingBlock "block1" (gameWidth / 2 - 100, 350) (100, 57) 0 0 (-180) 9
               , oscillatingBlock "block2" (gameWidth / 2 - 200, 350) (100, 57) 0 0 (-180) 9
               , oscillatingBlock "block3" (gameWidth / 2 - 300, 350) (100, 57) 0 0 (-180) 9
               , oscillatingBlock "block4" (gameWidth / 2 - 400, 350) (100, 57) 0 0 (-180) 9
               , oscillatingBlock "block5" (gameWidth / 2      , 350) (100, 57) 0 0 (-180) 9
               , oscillatingBlock "block6" (gameWidth / 2 + 100, 350) (100, 57) 0 0 (-180) 9
               , oscillatingBlock "block7" (gameWidth / 2 + 200, 350) (100, 57) 0 0 (-180) 9
               , oscillatingBlock "block8" (gameWidth / 2 + 300, 350) (100, 57) 0 0 (-180) 9
               ]

objBlocks 18 = [ oscillatingBlock "block1" (              0, 275) (100, 57) 0 0 (-100) 9
               , oscillatingBlock "block2" (            100, 250) (100, 57) 0 0 (-75)  9
               , oscillatingBlock "block3" (            200, 225) (100, 57) 0 0 (-50)  9
               , oscillatingBlock "block4" (            300, 200) (100, 57) 0 0 (-25)  9
               , oscillatingBlock "block5" (gameWidth - 100, 275) (100, 57) 0 0 (-100) 9
               , oscillatingBlock "block6" (gameWidth - 200, 250) (100, 57) 0 0 (-75)  9
               , oscillatingBlock "block7" (gameWidth - 300, 225) (100, 57) 0 0 (-50)  9
               , oscillatingBlock "block8" (gameWidth - 400, 200) (100, 57) 0 0 (-25)  9
               ]
objBlocks 19 = [ staticBlock "block1" ((gameWidth - 57)/2, 120) (57, 100)
               , staticBlock "block2" ((gameWidth - 57)/2, 220) (57, 100)
               , staticBlock "block3" ((gameWidth - 57)/2, 320) (57, 100)
               , staticBlock "block4" ((gameWidth - 57)/2, 420) (57, 100)
               ]
objBlocks 20 = [ staticBlock "block11" (100, 120)             (57, 100)
               , staticBlock "block12" (100, 220)             (57, 100)
               , staticBlock "block13" (100, 320)             (57, 100)
               , staticBlock "block14" (100, 420)             (57, 100)
               , staticBlock "block21" (300, 120)             (57, 100)
               , staticBlock "block22" (300, 220)             (57, 100)
               , staticBlock "block23" (300, 320)             (57, 100)
               , staticBlock "block24" (300, 420)             (57, 100)
               , staticBlock "block41" (gameWidth - 157, 120) (57, 100)
               , staticBlock "block42" (gameWidth - 157, 220) (57, 100)
               , staticBlock "block43" (gameWidth - 157, 320) (57, 100)
               , staticBlock "block44" (gameWidth - 157, 420) (57, 100)
               , staticBlock "block51" (gameWidth - 357, 120) (57, 100)
               , staticBlock "block52" (gameWidth - 357, 220) (57, 100)
               , staticBlock "block53" (gameWidth - 357, 320) (57, 100)
               , staticBlock "block54" (gameWidth - 357, 420) (57, 100)
               ]
objBlocks 21 = [ oscillatingBlock "block11" ((gameWidth - 57) / 2, 120) (57, 100) 0 0 100 10
               , oscillatingBlock "block12" ((gameWidth - 57) / 2, 220) (57, 100) 0 0 100 10
               , oscillatingBlock "block13" ((gameWidth - 57) / 2, 320) (57, 100) 0 0 100 10
               , oscillatingBlock "block14" ((gameWidth - 57) / 2, 420) (57, 100) 0 0 100 10
               , oscillatingBlock "block15" ((gameWidth - 57) / 2, 520) (57, 100) 0 0 100 10
               ]
objBlocks 22 = [ oscillatingBlock "block11" (100, 120)             (57, 100) 0 0 100 10
               , oscillatingBlock "block12" (100, 220)             (57, 100) 0 0 100 10
               , oscillatingBlock "block13" (100, 320)             (57, 100) 0 0 100 10
               , oscillatingBlock "block14" (100, 420)             (57, 100) 0 0 100 10
               , oscillatingBlock "block15" (100, 520)             (57, 100) 0 0 100 10
               , oscillatingBlock "block41" (gameWidth - 157, 120) (57, 100) 0 0 100 10
               , oscillatingBlock "block42" (gameWidth - 157, 220) (57, 100) 0 0 100 10
               , oscillatingBlock "block43" (gameWidth - 157, 320) (57, 100) 0 0 100 10
               , oscillatingBlock "block44" (gameWidth - 157, 420) (57, 100) 0 0 100 10
               , oscillatingBlock "block45" (gameWidth - 157, 520) (57, 100) 0 0 100 10
               ]
objBlocks 23 = [ oscillatingBlock "block11" (100,      167)             (57, 100) 0 0 100 10
               , oscillatingBlock "block12" (300 - 57, 167)             (57, 100) 0 0 100 10
               , oscillatingBlock "block13" (100,      110)             (100, 57) 0 0 100 10
               , oscillatingBlock "block14" (200,      110)             (100, 57) 0 0 100 10
               , oscillatingBlock "block21" (gameWidth - 300,      167) (57, 100) 0 0 100 10
               , oscillatingBlock "block22" (gameWidth - 100 - 57, 167) (57, 100) 0 0 100 10
               , oscillatingBlock "block23" (gameWidth - 300,      110) (100, 57) 0 0 100 10
               , oscillatingBlock "block24" (gameWidth - 200,      110) (100, 57) 0 0 100 10
               ]
objBlocks 24 = [ staticBlock       "block11" (000,      267)             (57, 100) 
               , staticBlock       "block12" (200 - 57, 267)             (57, 100)
               , disappearingBlock "block13" (000,      210)             (100, 57) 8 
               , disappearingBlock "block14" (100,      210)             (100, 57) 8 
               , staticBlock       "block21" (200,      397)             (57, 100)
               , staticBlock       "block22" (400 - 57, 397)             (57, 100)
               , disappearingBlock "block23" (200,      340)             (100, 57) 15
               , disappearingBlock "block24" (300,      340)             (100, 57) 15
               , staticBlock       "block31" (gameWidth - 400,      397) (57, 100)
               , staticBlock       "block32" (gameWidth - 200 - 57, 397) (57, 100)
               , disappearingBlock "block33" (gameWidth - 400,      340) (100, 57) 15
               , disappearingBlock "block34" (gameWidth - 300,      340) (100, 57) 15
               , staticBlock       "block41" (gameWidth - 200,      267) (57, 100)
               , staticBlock       "block42" (gameWidth - 000 - 57, 267) (57, 100)
               , disappearingBlock "block43" (gameWidth - 200,      210) (100, 57) 8
               , disappearingBlock "block44" (gameWidth - 100,      210) (100, 57) 8
               ]
objBlocks 25 = [ disappearingBlock "block11" (80, 0)               (57, 100) 10
               , disappearingBlock "block12" (80, 100)             (57, 100) 10
               , staticBlock       "block13" (80, 200)             (57, 100)
               , staticBlock       "block14" (80, 300)             (57, 100)
               , disappearingBlock "block21" (260, 0)               (57, 100) 5
               , disappearingBlock "block22" (260, 100)             (57, 100) 5
               , staticBlock       "block23" (260, 200)             (57, 100)
               , staticBlock       "block24" (260, 300)             (57, 100)
               , disappearingBlock "block41" (gameWidth - 137, 0)   (57, 100) 10
               , disappearingBlock "block42" (gameWidth - 137, 100) (57, 100) 10
               , staticBlock       "block43" (gameWidth - 137, 200) (57, 100)
               , staticBlock       "block44" (gameWidth - 137, 300) (57, 100)
               , disappearingBlock "block51" (gameWidth - 317, 0)   (57, 100) 5
               , disappearingBlock "block52" (gameWidth - 317, 100) (57, 100) 5
               , staticBlock       "block53" (gameWidth - 317, 200) (57, 100)
               , staticBlock       "block54" (gameWidth - 317, 300) (57, 100)
               ]
objBlocks 26 = [ disappearingBlock "block11" (80, 0)                (57, 100) 5 
               , disappearingBlock "block12" (80, 100)              (57, 100) 5 
               , staticBlock       "block13" (80, 200)              (57, 100)
               , staticBlock       "block14" (80, 300)              (57, 100)
               , disappearingBlock "block21" (260, 0)               (57, 100) 10
               , disappearingBlock "block22" (260, 100)             (57, 100) 10
               , staticBlock       "block23" (260, 200)             (57, 100)
               , staticBlock       "block24" (260, 300)             (57, 100)
               , disappearingBlock "block41" (gameWidth - 137, 0)   (57, 100) 15
               , disappearingBlock "block42" (gameWidth - 137, 100) (57, 100) 15
               , staticBlock       "block43" (gameWidth - 137, 200) (57, 100)
               , staticBlock       "block44" (gameWidth - 137, 300) (57, 100)
               , disappearingBlock "block51" (gameWidth - 317, 0)   (57, 100) 20
               , disappearingBlock "block52" (gameWidth - 317, 100) (57, 100) 20
               , staticBlock       "block53" (gameWidth - 317, 200) (57, 100)
               , staticBlock       "block54" (gameWidth - 317, 300) (57, 100)
               ]
objBlocks 27 = [ oscillatingBlock "block11" (gameWidth / 2 - 350, 287)      (57, 100) 0 0 100 10
               , oscillatingBlock "block12" (gameWidth / 2 + 350 - 57, 287) (57, 100) 0 0 100 10
               , oscillatingBlock "block14" (gameWidth / 2 - 350, 230)      (100, 57) 0 0 100 10
               , oscillatingBlock "block15" (gameWidth / 2 - 250, 230)      (100, 57) 0 0 100 10
               , oscillatingBlock "block16" (gameWidth / 2 - 150, 230)      (100, 57) 0 0 100 10
               , oscillatingBlock "block17" (gameWidth / 2 -  50, 230)      (100, 57) 0 0 100 10
               , oscillatingBlock "block18" (gameWidth / 2 +  50, 230)      (100, 57) 0 0 100 10
               , oscillatingBlock "block19" (gameWidth / 2 + 150, 230)      (100, 57) 0 0 100 10
               , oscillatingBlock "block20" (gameWidth / 2 + 250, 230)      (100, 57) 0 0 100 10
               ]
objBlocks 28 = [ staticBlock "block11" (0,   450-57*0) (100, 50)
               , staticBlock "block12" (100, 450-57*1) (100, 50)
               , staticBlock "block14" (200, 450-57*2) (100, 57)
               , staticBlock "block15" (300, 450-57*3) (100, 57)
               , staticBlock "block16" (400, 450-57*4) (100, 57)
               , staticBlock "block17" (500, 450-57*5) (100, 57)
               ]
objBlocks 29 = [ sinusoidalBlock "block1"  (              0, 300) (100, 57) 0 0 5 (-100) 6 (6/10)
               , sinusoidalBlock "block2"  (            100, 300) (100, 57) 0 0 5 (-100) 6 (6*2/10)
               , sinusoidalBlock "block3"  (            200, 300) (100, 57) 0 0 5 (-100) 6 (6*3/10)
               , sinusoidalBlock "block4"  (            300, 300) (100, 57) 0 0 5 (-100) 6 (6*4/10)
               , sinusoidalBlock "block5"  (            400, 300) (100, 57) 0 0 5 (-100) 6 (6*5/10)
               , sinusoidalBlock "block6"  (gameWidth - 500, 300) (100, 57) 0 0 5 (-100) 6 (6*6/10)
               , sinusoidalBlock "block7"  (gameWidth - 400, 300) (100, 57) 0 0 5 (-100) 6 (6*7/10)
               , sinusoidalBlock "block8"  (gameWidth - 300, 300) (100, 57) 0 0 5 (-100) 6 (6*8/10)
               , sinusoidalBlock "block9"  (gameWidth - 200, 300) (100, 57) 0 0 5 (-100) 6 (6*9/10)
               , sinusoidalBlock "block10" (gameWidth - 100, 300) (100, 57) 0 0 5 (-100) 6 (6*10/10)
               ]

objBlocks _ = [ staticBlock      "block1" (200, 200) (100, 57) ]

-- | Four walls around the scene.
objWalls :: Int -> [AliveObject]
objWalls _ = [ inertSF objSideRight
             , inertSF objSideTop
             , inertSF objSideLeft
             , inertSF objSideBottom
             ]
