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
import Physics.CollisionEngine

-- Internal iports
import Game.Constants
import Game.GameState
import Game.Levels
import Game.Time
import Game.Input
import Game.Objects
import Game.ObjectSF

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
    outOfEnemies gs | any isBall (gameObjects gs) = NoEvent
                    | otherwise                   = Event gs

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
  proc (input, (cs, _energy)) -> do
     -- Adapt Input
     let oi = ObjectInput input cs

     -- Step
     -- Each obj processes its movement forward
     ol  <- dlSwitch objs -< oi
     let cs' = detectCollisions ol

     let energyLeft = maybe 0 playerEnergy (findPlayer ol)

     -- Output
     tLeft   <- time -< ()
     returnA -< ((ol, tLeft), (cs', energyLeft))

-- * Initial game objects
--
-- | Objects initially present: player, enemies, blocks and walls
--
--   Care should be taken so that objects do not overlap at the beginning of
--   any level.
initialObjects :: Int -> [AliveObject]
initialObjects lvl =
  concat [objEnemies lvl, objBlocks lvl, objPlayers lvl, objWalls lvl]
