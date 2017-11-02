{-# LANGUAGE Arrows     #-}
{-# LANGUAGE MultiWayIf #-}
-- | This module defines the game as a big Signal Function that transforms a
-- Signal carrying a Input 'Controller' information into a Signal carrying
-- 'GameState'.
--
-- There is no randomness in the game, the only input is the user's.
-- 'Controller' is an abstract representation of a basic input device with
-- position information and a /fire/ button.
--
-- The output is defined in 'GameState', and consists of basic information
-- (points, current level, etc.) and a universe of objects.
--
-- Objects are represented as Signal Functions as well ('ObjectSF'). In this
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
module Game.GamePlay
  -- (wholeGame)
  where

-- External imports
import FRP.Yampa
import FRP.Yampa.Extra
import FRP.Yampa.Switches

import Physics.CollisionEngine

-- Internal iports
import Game.Constants
import Game.GameState
import Game.Time
import Game.Input
import Game.Objects
import Game.ObjectSF
import Game.Objects.Walls
import Game.Objects.Player
import Game.Objects.Balls
import Game.Objects.Blocks

-- * General state transitions

-- | Generate the game signal from the user input.

-- Internally, this is a game in which the player can lose, until ('switch')
-- the player is completely dead, and then restart the game.

wholeGame :: SF Controller GameState
wholeGame = forgetPast $ 
   switch (level initialLevel >>> (identity &&& playerDead))
          (\_ -> wholeGame)

-- * Game over

-- | Detect the player in the game state is dead (SF).
playerDead :: SF GameState (Event ())
playerDead = playerDead' ^>> edge
  where
    -- | True if the player in the game state is dead (no lives left).
    playerDead' :: GameState -> Bool
    playerDead' gs = gamePlaying && dead
      where
        -- Dead in the game if not present, or if found dead
        dead = not (any isPlayer (gameObjects gs))
            || any playerIsDead (gameObjects gs)
    
        -- Player dead if it has no more lives left
        playerIsDead o = case objectProperties o of
          (PlayerProps _ lives _ _) -> lives < 0
          _                         -> False
    
        -- This is only defined when the game is in progress.
        gamePlaying = GamePlaying == gameStatus (gameInfo gs)

-- * Loading levels

-- | Show loading screen for 2 seconds, then plays the game.
level :: Int -> SF Controller GameState
level n =   levelLoading n >? after 2 () -- show loading screen for 2 seconds
        ||> levelCore    n >? arr outOfEnemies
        ||> level (n+1)
  where
    -- | Detect when there are no more enemies in the scene.
    outOfEnemies :: GameState -> (Event GameState)
    outOfEnemies gs | none isBall (gameObjects gs) = Event gs
                    | otherwise                    = NoEvent

-- | Produce a constant game state of loading a particular level.
levelLoading :: Int -> SF Controller GameState
levelLoading n = constant (GameState [] (GameInfo 0 n GameLoading))

-- | Play one level indefinitely (it never ends or restarts).
levelCore :: Int -> SF Controller GameState
levelCore =
  timeTransformSF timeProgressionReverse . limitHistory 5 . levelCoreForward

  -- checkpoint $ proc (c) -> do
  -- take    <- edge <<^ controllerCheckPointSave -< c
  -- restore <- edge <<^ controllerCheckPointRestore -< c
  -- g       <- levelCore' n -< c
  -- returnA -< (g, take, restore)

-- | Play one level indefinitely (it never ends or restarts), in forward time
--   direction.
levelCoreForward :: Int -> SF Controller GameState
levelCoreForward n = gamePlay (initialObjects n) >>^ composeGameState
  where
    -- Compose GameState output from 'gamePlay's output
    composeGameState :: (Objects, Time) -> GameState
    composeGameState (objs, t) = GameState objs (GameInfo t n GamePlaying)

-- * Partial game state

-- | Given an initial list of objects, it runs the game, presenting the output
-- from those objects at all times, notifying any time the ball hits the floor,
-- and and of any additional points made.
--
-- This works by using an initial accumulator with no collisions, passed to
-- a function that also produces a signal for new collisions.
gamePlay :: [AliveObject]                  -- ^ Initial game objects
         -> SF Controller (Objects, Time)  -- ^ Game input ~> Objects x time left
gamePlay objs = loopPre ([], 0) $ clocked gameTimeSF (gamePlayInternal objs)
  where 
    -- Generate time deltas, taking time direction and reversing power into
    -- account.
    gameTimeSF = proc (_, (_, e)) -> do
      dt <- deltas -< ()
      let dt' = if e < 0 && dt < 0 then (-dt) else dt
      returnA -< dt'

-- | Given an initial list of objects, it runs the game, presenting the output
-- from those objects at all times, notifying any time the ball hits the floor,
-- and and of any additional points made.
--
-- This function takes the collisions in an accumulator as input, and returns
-- the new collisions after each step.
--
-- The second value in the accumulator is the energy left.

gamePlayInternal
  :: [AliveObject]                                         -- ^ Initial game objects
  ->  SF (Controller,      (Game.Objects.Collisions, Int))
         ((Objects, Time), (Game.Objects.Collisions, Int)) -- ^ Input x collisions
                                                           --         x energy
                                                           --   ~> Objects
                                                           --         x time left
                                                           --         x collisions
                                                           --         x energy
     

gamePlayInternal objs = 
  proc (input, (cs, el)) -> do
     -- Adapt Input
     let oi = ObjectInput input cs

     -- Step
     -- Each obj processes its movement forward
     ol  <- dlSwitch objs -< oi
     let cs' = detectCollisions ol

     let eleft = playerEnergyObjs ol

     -- Output
     tLeft   <- time -< ()
     returnA -< ((ol, tLeft), (cs', eleft))

-- * Initial game objects
--
-- | Objects initially present: player, enemies, blocks and walls
--
--   Care should be taken so that objects do not overlap at the beginning of
--   any level.
initialObjects :: Int -> [AliveObject]
initialObjects level =
  objEnemies level ++ objBlocks level ++ objPlayers level ++ objWalls level

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
objEnemies n =
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
objBlocks 9  = [ sinusoidalBlock "block1"  (100, 300) (100, 57) 800 10 0 100 2 ]
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
objBlocks 20 = [ staticBlock "block11" (100, 120)       (57, 100)
               , staticBlock "block12" (100, 220)       (57, 100)
               , staticBlock "block13" (100, 320)       (57, 100)
               , staticBlock "block14" (100, 420)       (57, 100)
               , staticBlock "block21" (300, 120) (57, 100)
               , staticBlock "block22" (300, 220) (57, 100)
               , staticBlock "block23" (300, 320) (57, 100)
               , staticBlock "block24" (300, 420) (57, 100)
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

objBlocks n = [ staticBlock      "block1" (200, 200) (100, 57) ]

-- | Four walls around the scene.
objWalls :: Int -> [AliveObject]
objWalls _ = [ inertSF objSideRight
             , inertSF objSideTop
             , inertSF objSideLeft
             , inertSF objSideBottom
             ]

-- * Auxiliary functions

-- ** Game aux

-- | Safe function to get the energy of the player from the game state.
playerEnergyObjs :: Objects -> Int
playerEnergyObjs objs = maybe 0 playerEnergy (findPlayer objs)

-- ** Other aux

-- | 'True' if property does not hold for any element, 'False' otherwise.
none :: Foldable t => (a -> Bool) -> t a -> Bool
none p = not . any p
