{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Arrows     #-}
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
module Game (wholeGame) where

-- External imports
import Prelude hiding (id, (.))
import Control.Category (id, (.))
import Data.List
import Data.Maybe
import Debug.Trace
import FRP.Yampa -- as Yampa
-- import FRP.Yampa.InternalCore
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

-- | Run the game that the player can lose at until ('switch') the player is
-- completely dead, and then restart the game.
wholeGame :: SF Controller GameState
wholeGame = forgetPast $ 
   switch (level 0 >>> (identity &&& playerDead))
                     (\_ -> wholeGame)

-- * Game over

-- | Detect the death of a player by searching for it in the scene (SF).
playerDead :: SF GameState (Event ())
playerDead = playerDead' ^>> edge

-- | Detect the death of a player by searching for it in the scene.
playerDead' :: GameState -> Bool
playerDead' gs = gamePlaying && dead
 where
   -- Dead in the game if not present, or if found dead
   dead = null (filter isPlayer (gameObjects gs))
       || not (null (filter playerIsDead (gameObjects gs)))

   -- Player dead if it has no more lives left
   playerIsDead o = case objectKind o of
     (Player _ lives _ _) -> lives < 0
     otherwise            -> False

   -- This is only defined when the game is in progress.
   gamePlaying = GamePlaying == gameStatus (gameInfo gs)

-- | Show loading screen for 2 seconds, then move on to play
-- the game.
level :: Int -> SF Controller GameState
level n = switch
  (levelLoading n &&& after 2 ()) -- show loading screen for 2 seconds
  (\_ -> levelLoaded n)

-- | Play a level till completed, then move on to the next level.
levelLoaded :: Int -> SF Controller GameState
levelLoaded n = switch
  (playLevel n >>> (identity &&& outOfEnemies))
  (\_ -> level (n + 1))

timeProgression :: SF Controller (DTime -> DTime)
timeProgression = -- slowDown
  proc (c) -> do
   let rev  = if controllerReverse c then ((-1)*) else id
   returnA -< rev

slowDown :: SF Controller (DTime -> DTime)
slowDown = proc (c) -> do
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


timeProgression' :: SF ObjectInput (DTime -> DTime)
timeProgression' = arr userInput >>> stopClock

stopClock :: SF Controller (DTime -> DTime)
stopClock = switch (arr controllerHalt >>> arr (\c' -> if c' then (const 0, Event ()) else (id, noEvent)))
                   (\_ -> switch (constant (const 0) &&& after 25 ())
                                 (\_ -> stopClock))

-- | Produce a constant game state of loading a particular level.
levelLoading :: Int -> SF a GameState
levelLoading n = constant (GameState [] (GameInfo 0 n GameLoading))

-- | Play one level indefinitely (it never ends or restarts).
playLevel :: Int -> SF Controller GameState
playLevel n =  playLevel' n 
  -- checkpoint $ proc (c) -> do
  -- take    <- edge <<^ controllerCheckPointSave -< c
  -- restore <- edge <<^ controllerCheckPointRestore -< c
  -- g       <- playLevel' n -< c
  -- returnA -< (g, take, restore)

playLevel' :: Int -> SF Controller GameState
playLevel' n =  timeTransformSF timeProgression $ limitHistory 5 $ playLevel'' n

playLevel'' :: Int -> SF Controller GameState
playLevel'' n = gamePlay (initialObjects n) >>^ composeGameState
  where
    -- Compose GameState output from 'gamePlay's output
    composeGameState :: (Objects, Time) -> GameState
    composeGameState (objs, t) = GameState objs (GameInfo t n GamePlaying)

-- | Detect when there are no more enemies in the scene.
outOfEnemies :: SF GameState (Event GameState)
outOfEnemies = arr outOfEnemies'
 where
   outOfEnemies' :: GameState -> (Event GameState)
   outOfEnemies' gs | null balls = Event gs
                    | otherwise  = NoEvent
     where
       balls = filter isBall (gameObjects gs)

-- ** Game with partial state information

-- | Given an initial list of objects, it runs the game, presenting the output
-- from those objects at all times, notifying any time the ball hits the floor,
-- and and of any additional points made.
--
-- This works as a game loop with a post-processing step. It uses
-- a well-defined initial accumulator and a traditional feedback
-- loop.
--
-- The internal accumulator holds the last known collisions (discarded at every
-- iteration).

playerEnergy'' :: Objects -> Int
playerEnergy'' objs = 
  let p = findPlayer objs
  in case p of
      Just p' -> playerEnergy p'
      Nothing -> 0

gameTimeSF = proc (_, (_, e)) -> do
   dt <- deltas -< ()
   let dt' = if e < 0 && dt < 0 then (-dt) else dt
   returnA -< dt'

gamePlay :: [ListSF ObjectInput Object] -> SF Controller (Objects, Time)
gamePlay objs = loopPre ([], 0) $ clocked gameTimeSF (gamePlay' objs)
  -- Process physical movement and detect new collisions
     -- -- Adapt Input
     -- let oi = ObjectInput input cs

     -- -- Step
     -- -- Each obj processes its movement forward
     -- ol  <- dlSwitch objs -< oi
     -- let cs' = detectCollisions ol

     -- let energyLeft = playerEnergy'' ol

     -- -- Output
     -- tLeft   <- time -< ()
     -- returnA -< ((ol, tLeft), (cs', energyLeft))

-- gamePlay' :: SF (Controller, (Collisions, Int)) ((Objects, Time), (Collisions, Int))
gamePlay' :: [ListSF ObjectInput Object]
          -> SF (Controller, (Objects.Collisions, Int))
                (([Object], Time), (Collisions.Collisions String, Int))
gamePlay' objs = 
  proc (input, (cs, el)) -> do
     -- Adapt Input
     let oi = ObjectInput input cs

     -- Step
     -- Each obj processes its movement forward
     ol  <- dlSwitch objs -< oi
     let cs' = detectCollisions ol

     let eleft = playerEnergy'' ol

     -- Output
     tLeft   <- time -< ()
     returnA -< ((ol, tLeft), (cs', eleft))

-- * Game objects
--
-- | Objects initially present: the walls, the ball, the player and the blocks.
initialObjects :: Int -> [ListSF ObjectInput Object]
initialObjects level =
  objEnemies level ++ blocks level ++ objPlayers ++ walls
 where
   walls = [ inertSF objSideRight
           , inertSF objSideTop
           , inertSF objSideLeft
           , inertSF objSideBottom
           ]

-- ** Enemies

-- | Defines the enemies depending on the level.
--
-- This function is paired with 'blocks', because there could be inconsistent
-- initial positions in which blocks and enemies already overlap.
--
-- WARNING: All objects need different names, both at the beginning and during
-- gameplay.

objEnemies :: Int -> [ListSF ObjectInput Object]

objEnemies 0 =
  [ splittingBall ballWidth "ballEnemy1" (600, 300) (360, -350) ]

objEnemies 1 =
  [ splittingBall ballMedium "ballEnemy1" (width/4, 300)   (360, -350)
  , splittingBall ballMedium "ballEnemy2" (3*width/4, 300) (360, -350) ]

objEnemies 2 =
  map ballLeft [1..4] ++ map ballRight [1..4]
 where baseL = 20
       sep   = width / 20
       baseR = width - (baseL  + 4 * sep)

       ballLeft n = splittingBall ballSmall ("ballEnemyL" ++ show n)
                         (baseL + n * sep, 100) (-200, -200)

       ballRight n = splittingBall ballSmall ("ballEnemyR" ++ show n)
                           (baseR + n * sep, 100) (200, -200)

objEnemies n =
  [ splittingBall ballBig "ballEnemy1" (600, 300) (360, -350) ]

-- ** Blocks
--
-- Blocks are horizontal rectangles that /every/ other element collides
-- with. They need not be static.

-- | List of blocks depending on the level.
blocks :: Int -> [ListSF ObjectInput Object]
blocks 0 = [ objBlock    "block1" (200, 55)  (100, 57)               ]
blocks 1 = [ movingBlock "block1" (400, 200) (100, 57) 200 10   0  0 ]
blocks 2 = [ movingBlock "block1" (400, 200) (100, 57) 0    0 100 10 ]
blocks 3 = [ movingBlock "block1" (324, 200) (100, 57) 200  6   0  0
           , movingBlock "block2" (700, 200) (100, 57) 200  6 100 10
           ]
blocks n = [ objBlock    "block1" (200, 200) (100, 57) ]

-- *** Moving blocks

-- | A moving block with an initial position and size, and horizontal and
-- vertical amplitude and periods. If an amplitude is /not/ zero, the
-- block moves along that dimension using a periodic oscillator
-- (see the SF 'osci').

movingBlock :: String
            -> Pos2D -> Size2D  -- Geometry
            -> Double -> Double -- Horizontal oscillation amplitude and period
            -> Double -> Double -- Vertical   oscillation amplitude and period
            -> ListSF ObjectInput Object
movingBlock name (px, py) size hAmp hPeriod vAmp vPeriod = ListSF $ proc _ -> do
  px' <- vx -< px
  py' <- vy -< py
  returnA -< (Object { objectName           = name
                     , objectKind           = Block size
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

-- | Generic block builder, given a name, a size and its base
-- position.
objBlock :: ObjectName -> Pos2D -> Size2D -> ListSF ObjectInput Object
objBlock name pos size = ListSF $ timeTransformSF timeProgression' $ constant
  (Object { objectName           = name
          , objectKind           = Block size
          , objectPos            = pos
          , objectVel            = (0,0)
          , canCauseCollisions   = False
          , collisionEnergy      = 0
          }, False, [])

-- ** Enemy sizes
ballGiant  = ballWidth
ballBig    = ballGiant  / 2
ballMedium = ballBig    / 2
ballSmall  = ballMedium / 2

-- ** Player
objPlayers :: [ListSF ObjectInput Object]
objPlayers =
  [ player initialLives playerName (320, 20) True ]

-- ** Guns

gun :: String -> SF (ObjectInput, Pos2D) [ListSF ObjectInput Object]
gun name = normalGun name
  -- To switch between different kinds of guns
  -- gun name = switch
  --   (normalGun name &&& after 5 ())
  --   (\_ -> multipleGun name)

-- *** Normal gun, fires one shot at a time

normalGun :: String -> SF (ObjectInput, Pos2D) [ListSF ObjectInput Object]
normalGun name = revSwitch (constant [] &&& gunFired name)
                           (\fireLSF -> blockedGun name fireLSF)

blockedGun name fsf = revSwitch (([fsf] --> constant []) &&& fireDead fsf)
                             (\_ -> normalGun name)

fireDead fsf = proc (oi, _) -> do
  (_, b, _) <- listSF fsf -< oi
  justDied <- edge -< b
  returnA -< justDied

gunFired :: String -> SF (ObjectInput, Pos2D) (Event (ListSF ObjectInput Object))
gunFired name = proc (i, ppos) -> do
  -- Fire!!
  newF1  <- edge -< controllerClick (userInput i)
  uniqId <- (\t -> "fire" ++ name ++ show t) ^<< time -< ()

  let newFire = fire uniqId (fst ppos + playerWidth / 2, 0) False
  returnA -< newF1 `tag` newFire

eventToList :: Event a -> [a]
eventToList NoEvent   = []
eventToList (Event a) = [a]

-- *** Normal gun, fires one shot at a time
multipleGun :: String -> SF (ObjectInput, Pos2D) [ListSF ObjectInput Object]
multipleGun name = eventToList ^<< gunFired name

player :: Int -> String -> Pos2D -> Bool -> ListSF ObjectInput Object
player lives name p0 vul = ListSF $ proc i -> do
  (ppos, pvel) <- playerProgress name p0 -< i

  let state = playerState (userInput i)

  -- newF1  <- isEvent ^<< edge                          -< controllerClick (userInput i)
  -- uniqId <- (\t -> "fire" ++ name ++ show t) ^<< time -< ()
  -- let newF1Arrows = [ fire uniqId (fst ppos, 0) False
  --                   | newF1 ]

  newF1Arrows <- gun name -< (i, ppos)

  -- Dead?
  let hitByBall = not $ null
                $ collisionMask name ("ball" `isPrefixOf`)
                $ collisions i

  vulnerable <- alwaysForward $ 
                  switch (constant vul &&& after 2 ())
                         (\_ -> constant True) -< ()

  dead <- isEvent ^<< edge -< hitByBall && vulnerable

  let newPlayer   = [ player (lives-1) name p0 False
                    | dead  && lives > 0 ]

  dt <- deltas -< ()
  energy <- loopPre 5 (arr (dup . max 0 . min 5 . sumTime)) -< dt
  --  max 0 (min 5 (round (fromIntegral (playerEnergy'' ol) + dt)))

  -- Final player
  returnA -< (Object { objectName           = name
                     , objectKind           = Player state lives vulnerable (round energy)
                     , objectPos            = ppos
                     , objectVel            = pvel
                     , canCauseCollisions   = True
                     , collisionEnergy      = 1
                     }
             , dead
             , newF1Arrows ++ newPlayer)

sumTime :: (DTime, DTime) -> DTime
sumTime (dt, e) = e + dt

playerState :: Controller -> PlayerState
playerState controller =
  case (controllerLeft controller, controllerRight controller, controllerClick controller) of
    (_,    _, True) -> PlayerShooting
    (True, _, _)    -> PlayerLeft
    (_,    True, _) -> PlayerRight
    _               -> PlayerStand

playerName :: String
playerName = "player"

playerProgress :: String -> Pos2D -> SF ObjectInput (Pos2D, Vel2D)
playerProgress pid p0 = proc i -> do
  -- Obtain velocity based on state and input, and obtain
  -- velocity delta to be applied to the position.
  v  <- repeatSF getVelocity PlayerStand -< userInput i

  let collisionsWithBlocks = filter onlyBlocks (collisions i)

      onlyBlocks (Collision cdata) = any (playerCollisionElem . fst) cdata

      playerCollisionElem s = isBlockId s || isWallId s
      isBlockId = ("block" `isPrefixOf`)
      isWallId  = ("Wall" `isSuffixOf`)

  let ev = changedVelocity pid collisionsWithBlocks
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
   stateChanged oldState = arr playerState >>> ifDiff oldState

-- *** Fire/arrows/bullets/projectiles

-- | This produces bullets that die when they hit the top of the screen.
-- There's sticky bullets and normal bullets. Sticky bullets get stuck for a
-- while before they die.
fire :: String -> Pos2D -> Bool -> ListSF ObjectInput Object
fire name (x0, y0) sticky = ListSF $ proc i -> do

  -- Calculate arrow tip
  yT <- (y0+) ^<< integral -< fireSpeed
  let y = min height yT

  -- Delay death if the fire is "sticky"
  hit <- revSwitch (never &&& fireHitCeiling) (\_ -> stickyDeath sticky) -< y

  hitBall  <- arr (fireCollidedWithBall  name) -< collisions i
  hitBlock <- arr (fireCollidedWithBlock name) -< collisions i

  let dead = isEvent hit || hitBall || hitBlock

  let object = Object { objectName = name
                      , objectKind = Projectile
                      , objectPos  = (x0, y)
                      , objectVel  = (0, 0)
                      , canCauseCollisions = True
                      , collisionEnergy = 0
                      }

  returnA -< (object, dead, [])

 where

   fireHitCeiling = arr (>= height) >>> edge
   fireCollidedWithBall  bid = not . null . collisionMask bid ("ball" `isPrefixOf`)
   fireCollidedWithBlock bid = not . null . collisionMask bid ("block" `isPrefixOf`)

stickyDeath :: Bool -> SF a (Event ())
stickyDeath True  = after 30 ()
stickyDeath False = constant (Event ())

-- *** Ball

splittingBall :: Double -> String -> Pos2D -> Vel2D -> ListSF ObjectInput Object
splittingBall size bid p0 v0 = ListSF $ timeTransformSF timeProgression' $ proc i -> do

  -- Default, just bouncing behaviour
  bo <- bouncingBall size bid p0 v0 -< i

  -- Hit fire? If so, it should split
  click <- edge <<^ ballCollidedWithFire bid -< collisions i
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
  let offspringL = splittingBall offspringSize offspringIDL bpos bvel
      offspringR = splittingBall offspringSize offspringIDR bpos ovel
      offspring  = if shouldSplit && not tooSmall
                    then [ offspringL, offspringR ]
                    else []

  -- If it splits, we just remove this one
  let dead = shouldSplit

  returnA -< (bo, dead, offspring)

ballCollidedWithFire :: ObjectName -> Objects.Collisions -> Bool
ballCollidedWithFire bid = not . null . collisionMask bid ("fire" `isPrefixOf`)

-- A bouncing ball moves freely until there is a collision, then bounces and
-- goes on and on.
--
-- This SF needs an initial position and velocity. Every time
-- there is a bounce, it takes a snapshot of the point of
-- collision and corrected velocity, and starts again.
--
bouncingBall :: Double -> String -> Pos2D -> Vel2D -> ObjectSF
bouncingBall size bid p0 v0 = repeatRevSF (progressAndBounce size bid) (p0, v0)


-- | Calculate the future tentative position, and bounce if necessary. Pass on
-- snapshot of ball position and velocity if bouncing.
progressAndBounce :: Double -> String -> (Pos2D, Vel2D)
                  -> SF ObjectInput (Object, Event (Pos2D, Vel2D))
progressAndBounce size bid (p0, v0) = proc i -> do

  -- Position of the ball, starting from p0 with velicity v0, since the
  -- time of last switching (or being fired, whatever happened last)
  -- provided that no obstacles are encountered.
  o <- freeBall size bid p0 v0 -< i

  -- The ballBounce needs the ball SF' input (which has knowledge of
  -- collisions), so we carry it parallely to the tentative new
  -- positions, and then use it to detect when it's time to bounce
  b <- ballBounce bid -< (i, o)

  returnA -< (o, b)

-- | Detect if the ball must bounce and, if so, take a snapshot of the object's
-- current position and velocity.
--
-- NOTE: To avoid infinite loops when switching, the initial input is discarded
-- and never causes a bounce. Careful: this prevents the ball from bouncing
-- immediately after creation, which may or may not be what we want.
ballBounce :: String -> SF (ObjectInput, Object) (Event (Pos2D, Vel2D))
ballBounce bid = noEvent --> ballBounce' bid

-- | Detect if the ball must bounce and, if so, take a snapshot of the object's
-- current position and velocity.
--
-- This does the core of the work, and does not ignore the initial input.
--
-- It proceeds by detecting whether any collision affects the ball's velocity,
-- and outputs a snapshot of the object position and the corrected velocity if
-- necessary.
ballBounce' :: String -> SF (ObjectInput, Object) (Event (Pos2D, Vel2D))
ballBounce' bid = proc (ObjectInput ci cs, o) -> do
  -- HN 2014-09-07: With the present strategy, need to be able to
  -- detect an event directly after
  -- ev <- edgeJust -< changedVelocity "ball" cs
  let collisionsWithoutBalls = filter (not . allBalls) cs
      allBalls (Collision cdata) = all (isPrefixOf "ball" . fst) cdata

  let collisionsWithoutPlayer = filter (not . anyPlayer)
                                 collisionsWithoutBalls
      anyPlayer (Collision cdata) = any (isPrefixOf "player" . fst) cdata

  let ev = maybeToEvent (changedVelocity bid collisionsWithoutPlayer)
  returnA -< fmap (\v -> (objectPos o, v)) ev

-- | Position of the ball, starting from p0 with velicity v0, since the time of
-- last switching (that is, collision, or the beginning of time --being fired
-- from the paddle-- if never switched before), provided that no obstacles are
-- encountered.
freeBall :: Double -> String -> Pos2D -> Vel2D -> ObjectSF
freeBall size name p0 v0 = proc (ObjectInput ci cs) -> do

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
                   , objectKind           = Ball size
                   , objectPos            = pos
                   , objectVel            = vel
                   , canCauseCollisions   = True
                   , collisionEnergy      = 1
                   }

  returnA -< obj
 where -- Spike every time the user presses the Halt key
       restartCond = spikeOn (arr controllerStop)

       -- Calculate the velocity, restarting when the user
       -- requests it.
       vdiffSF = proc (iv, acc, ci) -> do
                   -- Calculate velocity difference by integrating acceleration
                   -- Reset calculation when user requests to stop balls
                   vd <- restartOn (arr fst >>> integral)
                                   (arr snd >>> restartCond) -< (acc, ci)

                   -- Add initial velocity, and cap the result
                   v <- arr (uncurry (^+^)) -< (iv, vd)
                   let vFinal = limitNorm v (maxVNorm size)

                   returnA -< vFinal

       -- Initial velocity, reset when the user requests it.
       startAs v0  = revSwitch (constant v0 &&& restartCond)
                               (\_ -> startAs (0,0))
