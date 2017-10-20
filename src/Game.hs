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
-- Objects are represented as Signal Functions as well ('ObjectSF'). This
-- allows them to react to user input and change with time.  Each object is
-- responsible for itself, but it cannot affect others: objects can watch
-- others, depend on others and react to them, but they cannot /send a
-- message/ or eliminate other objects. However, if you would like to
-- dynamically introduce new elements in the game (for instance, falling
-- powerups that the player must collect before they hit the ground) then it
-- might be a good idea to allow objects not only to /kill themselves/ but
-- also to spawn new object.
--
-- This module contains two sections:
--
--   - A collection of gameplay SFs, which control the core game loop, carry
--   out collision detection, , etc.
--
--   - One SF per game object. These define the elements in the game universe,
--   which can observe other elements, depend on user input, on previous
--   collisions, etc.
--
-- You may want to read the basic definition of 'GameState', 'Controller' and
-- 'ObjectSF' before you attempt to go through this module.
--
module Game
  -- (wholeGame)
  where

-- External imports
import Prelude hiding (id, (.))
import Control.Category (id, (.))
import Data.List
import Data.Maybe
import Debug.Trace
import FRP.Yampa
import FRP.Yampa.Extra
import FRP.Yampa.Switches

-- General-purpose internal imports
import Data.Extra.Ord
import Data.Extra.VectorSpace
import Physics.Oscillator
import Physics.TwoDimensions.Collisions       as Collisions
import Physics.TwoDimensions.Dimensions
import Physics.TwoDimensions.GameCollisions
import Physics.TwoDimensions.Shapes
import Physics.TwoDimensions.PhysicalObjects

-- Internal iports
import Constants
import GameState
import Input
import Objects
import ObjectSF
import Objects.Walls

-- * General state transitions

-- | Generate the game signal from the user input.

-- Internally, this is a game in which the player can lose, until ('switch')
-- the player is completely dead, and then restart the game.

wholeGame :: SF Controller GameState
wholeGame = forgetPast $ 
   switch (level 0 >>> (identity &&& playerDead))
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
          otherwise                 -> False
    
        -- This is only defined when the game is in progress.
        gamePlaying = GamePlaying == gameStatus (gameInfo gs)

-- * Loading levels

-- | Show loading screen for 2 seconds, then plays the game.
level :: Int -> SF Controller GameState
level n = switch
  (levelLoading n &&& after 2 ()) -- show loading screen for 2 seconds
  (\_ -> levelCore n)

-- | Straight to play level until finished, then go on to next level.
levelCore :: Int -> SF Controller GameState
levelCore n = switch
    (playLevel n >>> (identity &&& arr outOfEnemies))
    (\_ -> level (n + 1))
  where
    -- | Detect when there are no more enemies in the scene.
    outOfEnemies :: GameState -> (Event GameState)
    outOfEnemies gs | none isBall (gameObjects gs) = Event gs
                    | otherwise                    = NoEvent

-- | Produce a constant game state of loading a particular level.
levelLoading :: Int -> SF Controller GameState
levelLoading n = constant (GameState [] (GameInfo 0 n GameLoading))

-- | Play one level indefinitely (it never ends or restarts).
playLevel :: Int -> SF Controller GameState
playLevel =
  timeTransformSF timeProgressionReverse . limitHistory 5 . playLevelForward

  -- checkpoint $ proc (c) -> do
  -- take    <- edge <<^ controllerCheckPointSave -< c
  -- restore <- edge <<^ controllerCheckPointRestore -< c
  -- g       <- playLevel' n -< c
  -- returnA -< (g, take, restore)

-- | Play one level indefinitely (it never ends or restarts), in forward time
--   direction.
playLevelForward :: Int -> SF Controller GameState
playLevelForward n = gamePlay (initialObjects n) >>^ composeGameState
  where
    -- Compose GameState output from 'gamePlay's output
    composeGameState :: (Objects, Time) -> GameState
    composeGameState (objs, t) = GameState objs (GameInfo t n GamePlaying)

-- * Time manipulation

-- | Time transformation that allows time to be reversed.
timeProgressionReverse :: SF Controller (DTime -> DTime)
timeProgressionReverse = proc (c) -> do
  -- NOTE: Another option is slowDown
  let rev  = if controllerReverse c then ((-1)*) else id
  returnA -< rev

-- | Time transformation that slows down time upon request.
timeProgressionSlowDown :: SF Controller (DTime -> DTime)
timeProgressionSlowDown = proc (c) -> do
  rec let slow = controllerReverse c
          unit = if | power' >= 0 && slow -> (-1)
                    | power' >= maxPower  -> 0
                    | otherwise           -> 1
      power <- (maxPower +) ^<< integral -< unit
      let power' = min maxPower (max 0 power)
          dtF    = if slow && (power' > 0) then (0.1*) else id
  returnA -< dtF
 where
   maxPower :: Double
   maxPower = 5

-- | Time transformation that can halt time for an object.
timeProgressionHalt :: SF ObjectInput (DTime -> DTime)
timeProgressionHalt =   constant id        &&& mustHalt
                    ||> constant (const 0) &&& after 25 ()
                    ||> timeProgressionHalt
 where
   mustHalt = (controllerHalt . userInput) ^>> edge

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
  :: [AliveObject]                                    -- ^ Initial game objects
  ->  SF (Controller,      (Objects.Collisions, Int))
         ((Objects, Time), (Objects.Collisions, Int)) -- ^ Input x collisions
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
objBlocks n = [ staticBlock      "block1" (200, 200) (100, 57) ]

-- | Four walls around the scene.
objWalls :: Int -> [AliveObject]
objWalls _ = [ inertSF objSideRight
             , inertSF objSideTop
             , inertSF objSideLeft
             , inertSF objSideBottom
             ]

-- * Game objects

-- ** Player

-- | A player with a given it, lives, position and initial vulnerability.
player :: ObjectName -> Int -> Pos2D -> Bool -> AliveObject
player name lives p0 vul = ListSF $ proc i -> do
  (ppos, pvel) <- playerMovement name p0 -< i

  let state = playerState (userInput i)

  -- newF1  <- isEvent ^<< edge                          -< controllerClick (userInput i)
  -- uniqId <- (\t -> "fire" ++ name ++ show t) ^<< time -< ()
  -- let newF1Arrows = [ fire uniqId (fst ppos, 0) False
  --                   | newF1 ]

  newF1Arrows <- playerGun name -< (i, ppos)

  -- Dead?
  let hitByBall = not $ null
                $ collisionMask (name, Player) (collisionObjectKind Ball)
                $ collisions i

  vulnerable <- alwaysForward $ 
                  switch (constant vul &&& after 2 ())
                         (\_ -> constant True) -< ()

  dead <- isEvent ^<< edge -< hitByBall && vulnerable

  let newPlayer   = [ player name (lives-1) p0 False
                    | dead  && lives > 0 ]

  dt <- deltas -< ()

  energy <- loopPre 5 (arr (dup . max 0 . min 5 . sumTime)) -< dt
  --  max 0 (min 5 (round (fromIntegral (playerEnergyObjs ol) + dt)))

  -- Final player
  returnA -< (Object { objectName           = name
                     , objectKind           = Player
                     , objectProperties     = PlayerProps state lives vulnerable (round energy)
                     , objectPos            = ppos
                     , objectVel            = pvel
                     , canCauseCollisions   = True
                     , collisionEnergy      = 1
                     }
             , dead
             , newF1Arrows ++ newPlayer)

  where 
    sumTime :: (DTime, DTime) -> DTime
    sumTime = uncurry (+)

-- | Movement of a player around the screen.
playerMovement :: ObjectName -> Pos2D -> SF ObjectInput (Pos2D, Vel2D)
playerMovement pid p0 = proc i -> do
  -- Obtain velocity based on state and input, and obtain
  -- velocity delta to be applied to the position.
  v  <- repeatSF getVelocity PlayerStand -< userInput i

  let collisionsWithBlocks :: Collisions.Collisions (ObjectName, ObjectKind)
      collisionsWithBlocks = filter onlyBlocks (collisions i)

      onlyBlocks :: Collisions.Collision (ObjectName, ObjectKind) -> Bool
      onlyBlocks (Collision cdata) = any (playerCollisionElem . fst) cdata

      playerCollisionElem s = isBlockId s || isWallId s
      isBlockId = collisionObjectKind Block
      isWallId  = collisionObjectKind Side

  let ev = changedVelocity (pid, Player) collisionsWithBlocks
      vc = fromMaybe v ev

  (px,py) <- (p0 ^+^) ^<< alwaysForward integral -< vc

  -- Calculate actual velocity based on corrected/capped position
  v' <- derivative -< (px, py)

  returnA -< ((px, py), v')

 where

   capPlayerPos (px, py) = (px', py')
     where px' = inRange (0, width - playerWidth)  px
           py' = inRange (0, height - playerHeight) py

   getVelocity :: PlayerState -> SF Controller (Vel2D, Event PlayerState)
   getVelocity pstate = stateVel pstate &&& stateChanged pstate

   stateVel :: PlayerState -> SF a Vel2D
   stateVel PlayerLeft     = constant (-playerSpeed, 0)
   stateVel PlayerRight    = constant (playerSpeed,  0)
   stateVel PlayerStand    = constant (0,            0)
   stateVel PlayerShooting = constant (0,            0)

   stateChanged :: PlayerState -> SF Controller (Event PlayerState)
   stateChanged oldState = playerState ^>> ifDiff oldState

-- | State of the player based in user input.
playerState :: Controller -> PlayerState
playerState controller
  | controllerClick controller = PlayerShooting
  | controllerLeft  controller = PlayerLeft
  | controllerRight controller = PlayerRight
  | otherwise                  = PlayerStand

-- ** Guns

-- | The player's gun. Guns can fire shots, so guns may include
--   more than one object.
playerGun :: ObjectName -> SF (ObjectInput, Pos2D) [AliveObject]
playerGun = singleShotGun
  -- To switch between different kinds of guns
  -- playerGun name = switch
  --   (singleShotGun name &&& after 5 ())
  --   (\_ -> multiShotGun name)

-- | Gun that can be fired once until the bullet hits the wall or a ball, and
--   then can be fired again.
singleShotGun :: ObjectName -> SF (ObjectInput, Pos2D) [AliveObject]
singleShotGun name = revSwitch (constant [] &&& gunFired name)
                               (\fireLSF -> blockedGun name fireLSF)

-- | Gun that can be fired multiple times.
multiShotGun :: ObjectName -> SF (ObjectInput, Pos2D) [AliveObject]
multiShotGun name = eventToList ^<< gunFired name

-- | Possible event carrying a projectile, triggered when the
--   gun has been fired.
gunFired :: ObjectName -> SF (ObjectInput, Pos2D) (Event AliveObject)
gunFired name = proc (i, ppos) -> do
  -- Fire!!
  newF1  <- edge -< controllerClick (userInput i)
  uniqId <- (\t -> "bullet" ++ name ++ show t) ^<< time -< ()

  let newFire = bullet uniqId (fst ppos + playerWidth / 2, 0) False
  returnA -< newF1 `tag` newFire

-- | Gun that cannot be fired until the current bullet hits
--   the ceiling or a ball.
blockedGun :: ObjectName -> AliveObject -> SF (ObjectInput, Pos2D) [AliveObject]
blockedGun name fsf = revSwitch (([fsf] --> constant []) &&& bulletDead fsf)
                                (\_ -> singleShotGun name)
  where
    bulletDead fsf = proc (oi, _) -> do
      (_, b, _) <- listSF fsf -< oi
      justDied  <- edge       -< b
      returnA -< justDied

-- | Fire \/ arrows \/ bullets \/ projectiles. If the third argument is
--   'False', they die when they hit the top of the screen. If the third
--   argument is 'True', they stuck for a while before they die.
bullet :: ObjectName -> Pos2D -> Bool -> AliveObject
bullet name (x0, y0) sticky = ListSF $ proc i -> do

  -- Calculate arrow tip
  yT <- (y0+) ^<< integral -< bulletSpeed
  let y = min height yT

  -- Delay death if the bullet is "sticky"
  hit <- revSwitch (never &&& bulletHitCeiling) (\_ -> stickyDeath sticky) -< y

  let hitBall  = bulletCollidedWithBall  name $ collisions i
  let hitBlock = bulletCollidedWithBlock name $ collisions i

  let dead = isEvent hit || hitBall || hitBlock

  let object = Object { objectName         = name
                      , objectKind         = Projectile
                      , objectProperties   = ProjectileProps
                      , objectPos          = (x0, y)
                      , objectVel          = (0, 0)
                      , canCauseCollisions = True
                      , collisionEnergy    = 0
                      }

  returnA -< (object, dead, [])

 where

   bulletHitCeiling = (>= height) ^>> edge
   bulletCollidedWithBall  bid = not . null . collisionMask (bid, Projectile) (collisionObjectKind Ball)
   bulletCollidedWithBlock bid = not . null . collisionMask (bid, Projectile) (collisionObjectKind Block)

   stickyDeath :: Bool -> SF a (Event ())
   stickyDeath True  = after 30 ()
   stickyDeath False = constant (Event ())

-- ** Balls

-- | A ball that splits in two when hit unless it is too small.
splittingBall :: ObjectName -> Double -> Pos2D -> Vel2D -> AliveObject
splittingBall bid size p0 v0 = ListSF $ timeTransformSF timeProgressionHalt $ proc i -> do

    -- Default, just bouncing behaviour
    bo <- bouncingBall bid size p0 v0 -< i

    -- Hit fire? If so, it should split
    click <- edge <<^ ballIsHit bid -< collisions i
    let shouldSplit = isEvent click

    -- We need two unique IDs so that collisions work
    t <- localTime -< ()
    let offspringIDL = bid ++ show t ++ "L"
        offspringIDR = bid ++ show t ++ "R"

    let enforceYPositive (x,y) = (x, abs y)

    -- Position and velocity of new offspring
    let bpos = physObjectPos bo
        bvel = enforceYPositive $ physObjectVel bo
        ovel = enforceYPositive $ (\(vx,vy) -> (-vx, vy)) bvel

    -- Offspring size, unless this ball is too small to split
    let tooSmall      = size <= (ballWidth / 8)
    let offspringSize = size / 2

    -- Calculate offspring, if any
    let offspringL = splittingBall offspringIDL offspringSize bpos bvel
        offspringR = splittingBall offspringIDR offspringSize bpos ovel
        offspring  = if shouldSplit && not tooSmall
                      then [ offspringL, offspringR ]
                      else []

    -- If it splits, we just remove this one
    let dead = shouldSplit

    returnA -< (bo, dead, offspring)

  where

    -- | Determine if a given fall has been hit by a bullet.
    ballIsHit :: ObjectName -> Objects.Collisions -> Bool
    ballIsHit bid = not . null . collisionMask (bid, Ball) (collisionObjectKind Projectile)

-- | A bouncing ball that moves freely until there is a collision, then bounces
-- and goes on and on.
--
-- This SF needs an initial position and velocity. Every time there is a
-- bounce, it takes a snapshot of the point of collision and corrected
-- velocity, and starts again.
--
bouncingBall :: ObjectName -> Double -> Pos2D -> Vel2D -> ObjectSF
bouncingBall bid size p0 v0 = repeatRevSF (progressAndBounce bid size) (p0, v0)

-- | Calculate the future tentative position, and bounce if necessary. Pass on
-- snapshot of ball position and velocity if bouncing.
progressAndBounce :: ObjectName -> Double -> (Pos2D, Vel2D)
                  -> SF ObjectInput (Object, Event (Pos2D, Vel2D))
progressAndBounce bid size (p0, v0) = proc i -> do

  -- Position of the ball, starting from p0 with velicity v0, since the
  -- time of last switching (or being fired, whatever happened last)
  -- provided that no obstacles are encountered.
  o <- freeBall bid size p0 v0 -< i

  -- The ballBounce needs the ball SF' input (which has knowledge of
  -- collisions), so we carry it parallely to the tentative new
  -- positions, and then use it to detect when it's time to bounce
  b <- ballBounce bid -< (i, o)

  returnA -< (o, b)

-- | Detect if the ball must bounce and, if so, take a snapshot of the object's
-- current position and velocity.
--
-- It proceeds by detecting whether any collision affects the ball's velocity,
-- and outputs a snapshot of the object position and the corrected velocity if
-- necessary.

-- NOTE: To avoid infinite loops when switching, the initial input is discarded
-- and never causes a bounce. Careful: this prevents the ball from bouncing
-- immediately after creation, which may or may not be what we want.
ballBounce :: ObjectName -> SF (ObjectInput, Object) (Event (Pos2D, Vel2D))
ballBounce bid = noEvent --> proc (ObjectInput ci cs, o) -> do
  -- HN 2014-09-07: With the present strategy, need to be able to
  -- detect an event directly after
  -- ev <- edgeJust -< changedVelocity "ball" cs
  let collisionsWithoutBalls = filter (not . allBalls) cs
      allBalls (Collision cdata) = all (collisionObjectKind Ball . fst) cdata

  let collisionsWithoutPlayer = filter (not . anyPlayer)
                                 collisionsWithoutBalls
      anyPlayer (Collision cdata) = any (collisionObjectKind Player . fst) cdata

  let ev = maybeToEvent (changedVelocity (bid, Ball) collisionsWithoutPlayer)
  returnA -< fmap (\v -> (objectPos o, v)) ev

-- | Position of the ball, starting from p0 with velicity v0, since the time of
-- last switching (that is, collision, or the beginning of time if never
-- switched before), provided that no obstacles are encountered.
freeBall :: ObjectName -> Double -> Pos2D -> Vel2D -> ObjectSF
freeBall name size p0 v0 = proc (ObjectInput ci cs) -> do

  -- Integrate acceleration, add initial velocity and cap speed. Resets both
  -- the initial velocity and the current velocity to (0,0) when the user
  -- presses the Halt key (hence the dependency on the controller input ci).
  vInit <- startAs v0 -< ci
  vel   <- vdiffSF    -< (vInit, (0, -1000.8), ci)

  -- Any free moving object behaves like this (but with
  -- acceleration. This should be in some FRP.NewtonianPhysics
  -- module)
  pos <- (p0 ^+^) ^<< integral -< vel

  let obj = Object { objectName           = name
                   , objectKind           = Ball
                   , objectProperties     = BallProps size
                   , objectPos            = pos
                   , objectVel            = vel
                   , canCauseCollisions   = True
                   , collisionEnergy      = 1
                   }

  returnA -< obj
 where
   -- Spike every time the user presses the Halt key
   restartCond = spikeOn (arr controllerStop)

   -- Calculate the velocity, restarting when the user
   -- requests it.
   vdiffSF = proc (iv, acc, ci) -> do
               -- Calculate velocity difference by integrating acceleration
               -- Reset calculation when user requests to stop balls
               vd <- restartOn (fst ^>> integral)
                               (snd ^>> restartCond) -< (acc, ci)

               -- Add initial velocity, and cap the result
               v <- arr (uncurry (^+^)) -< (iv, vd)
               let vFinal = limitNorm v (maxVNorm size)

               returnA -< vFinal

   -- Initial velocity, reset when the user requests it.
   startAs v0  = revSwitch (constant v0 &&& restartCond)
                           (\_ -> startAs (0,0))

-- ** Blocks

-- | Static block builder, given a name, a size and its base
-- position.
staticBlock :: ObjectName -> Pos2D -> Size2D -> AliveObject
staticBlock name pos size = ListSF $ timeTransformSF timeProgressionHalt $ constant
  (Object { objectName           = name
          , objectKind           = Block
          , objectProperties     = BlockProps size
          , objectPos            = pos
          , objectVel            = (0,0)
          , canCauseCollisions   = False
          , collisionEnergy      = 0
          }, False, [])

-- | Moving block with an initial position and size, and horizontal and
-- vertical amplitude and periods. If an amplitude is /not/ zero, the block
-- moves along that dimension using a periodic oscillator (see 'osci').
oscillatingBlock :: ObjectName
                 -> Pos2D -> Size2D  -- Geometry
                 -> Double -> Double -- Horizontal oscillation amplitude and period
                 -> Double -> Double -- Vertical   oscillation amplitude and period
                 -> AliveObject
oscillatingBlock name (px, py) size hAmp hPeriod vAmp vPeriod = ListSF $ proc _ -> do
  px' <- vx -< px
  py' <- vy -< py
  returnA -< (Object { objectName           = name
                     , objectKind           = Block
                     , objectProperties     = BlockProps size
                     , objectPos            = (px', py')
                     , objectVel            = (0,0)
                     , canCauseCollisions   = False
                     , collisionEnergy      = 0
                     }, False, [])

 where

   -- To avoid errors, we check that the amplitude is non-zero, otherwise
   -- just pass the given position along.
   vx :: SF Double Double
   vx = if hAmp /= 0 then (px +) ^<< osci hAmp hPeriod else identity

   -- To avoid errors, we check that the amplitude is non-zero, otherwise
   -- just pass the given position along.
   vy :: SF Double Double
   vy = if vAmp /= 0 then (py +) ^<< osci vAmp vPeriod else identity

-- * Auxiliary functions

-- ** Game aux

-- | Safe function to get the energy of the player from the game state.
playerEnergyObjs :: Objects -> Int
playerEnergyObjs objs = maybe 0 playerEnergy (findPlayer objs)

-- | Check if collision is of given type.
collisionObjectKind :: ObjectKind -> (ObjectName, ObjectKind) -> Bool
collisionObjectKind ok1 (_, ok2) = ok1 == ok2

-- ** Yampa aux

-- | Singleton if Event, empty list otherwise.
eventToList :: Event a -> [a]
eventToList NoEvent   = []
eventToList (Event a) = [a]

-- | Execute an sf until it fires an event, and then execute another SF.

-- TODO: Is there a better abstraction to write this?
-- Maybe use tasks?
infixr 2 ||>
(||>) sf sfC = switch sf (const sfC)

-- ** Other aux

-- | 'True' if property does not hold for any element, 'False' otherwise.
none :: Foldable t => (a -> Bool) -> t a -> Bool
none p = not . any p
