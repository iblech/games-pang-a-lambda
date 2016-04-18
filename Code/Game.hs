{-# LANGUAGE Arrows #-}
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
import FRP.Yampa
import FRP.Yampa.Switches

-- General-purpose internal imports
import Data.List
import Data.Extra.VectorSpace
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

-- * General state transitions

-- | Run the game that the player can lose at ('canLose'), until ('switch')
-- there are no more levels ('outOfLevels'), in which case the player has won
-- ('wonGame').
wholeGame :: SF Controller GameState
wholeGame = gamePlay initialObjects >>> composeGameState
 where composeGameState :: SF (Objects, Time) GameState
       composeGameState = arr (second GameInfo >>> uncurry GameState)

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

gamePlay :: [ListSF ObjectInput Object] -> SF Controller (Objects, Time)
gamePlay objs = loopPre [] $
  -- Process physical movement and detect new collisions
  proc (input, cs) -> do
     -- Adapt Input
     let oi = ObjectInput input cs

     -- Step
     -- Each obj processes its movement forward
     ol  <- dlSwitch objs -< oi
     let cs' = detectCollisions ol

     -- Output
     tLeft   <- time -< ()
     returnA -< ((ol, tLeft), cs')

-- * Game objects
--
-- | Objects initially present: the walls, the ball, the player and the blocks.
initialObjects :: [ListSF ObjectInput Object]
initialObjects =
  -- Walls
  [ inertSF objSideRight
  , inertSF objSideTop
  , inertSF objSideLeft
  , inertSF objSideBottom
  ]
  ++ objEnemies
  ++ objPlayers

-- ** Enemies
objEnemies :: [ListSF ObjectInput Object]
objEnemies =
  [ splittingBall ballWidth "ballEnemy1" (600, 300) (360, -350) ]

-- ** Player
objPlayers :: [ListSF ObjectInput Object]
objPlayers =
  [ player playerName (320, 20) ]

player :: String -> Pos2D -> ListSF ObjectInput Object
player name p0 = ListSF $ proc i -> do
  (ppos, pvel) <- playerProgress p0 -< userInput i
  let state = playerState (userInput i)

  -- Fire!!
  newF1  <- isEvent ^<< edge                          -< controllerClick (userInput i)
  uniqId <- (\t -> "fire" ++ name ++ show t) ^<< time -< ()
  let newF1Arrows = [ fire uniqId (fst ppos, 0) False
                    | newF1 ]

  -- Dead?
  let hitByBall = not $ null
                $ collisionMask name ("ball" `isPrefixOf`)
                $ collisions i
  dead <- isEvent ^<< edge -< hitByBall

  let newPlayer   = [ player name p0 | dead ]

  -- Final player
  returnA -< (Object { objectName           = name
                     , objectKind           = Player state
                     , objectPos            = ppos
                     , objectVel            = pvel
                     , canCauseCollisions   = True
                     , collisionEnergy      = 1
                     }
             , dead
             , newF1Arrows ++ newPlayer)

playerState :: Controller -> PlayerState
playerState controller =
  case (controllerLeft controller, controllerRight controller) of
    (True, _)    -> PlayerLeft
    (_,    True) -> PlayerRight
    _            -> PlayerStand

playerName :: String
playerName = "player"

playerProgress :: Pos2D -> SF Controller (Pos2D, Vel2D)
playerProgress p0 = proc (c) -> do
  -- Obtain velocity based on state and input
  v <- repeatSF getVelocity PlayerStand -< c

  p <- (p0 ^+^) ^<< integral -< v
  returnA -< (p, v)

 where

   getVelocity :: PlayerState -> SF Controller (Vel2D, Event PlayerState)
   getVelocity pstate = stateVel pstate &&& stateChanged pstate

   stateVel :: PlayerState -> SF a Vel2D
   stateVel PlayerLeft  = constant (-playerSpeed, 0)
   stateVel PlayerRight = constant (playerSpeed,  0)
   stateVel PlayerStand = constant (0,            0)

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
  hit <- switch (never &&& fireHitCeiling) (\_ -> stickyDeath sticky) -< y

  hitB <- arr (fireCollidedWithBall name) -< collisions i

  let dead = isEvent hit || hitB

  let object = Object { objectName = name
                      , objectKind = Projectile
                      , objectPos  = (x0, y)
                      , objectVel  = (0, 0)
                      , canCauseCollisions = True
                      , collisionEnergy = 0
                      }

  returnA -< (object, dead, [])

 where

   fireHitCeiling = arr (> height) >>> edge
   fireCollidedWithBall bid = not . null . collisionMask bid ("ball" `isPrefixOf`)

stickyDeath True  = after 30 ()
stickyDeath False = constant (Event ())

-- *** Ball

splittingBall :: Double -> String -> Pos2D -> Vel2D -> ListSF ObjectInput Object
splittingBall size bid p0 v0 = ListSF $ proc i -> do

  -- Default, just bouncing behaviour
  bo <- bouncingBall size bid p0 v0 -< i

  -- Hit fire? If so, it should split
  click <- edge <<^ ballCollidedWithFire bid -< collisions i
  let shouldSplit = isEvent click

  -- We need two unique IDs so that collisions work
  t <- localTime -< ()
  let offspringIDL = bid ++ show t ++ "L"
      offspringIDR = bid ++ show t ++ "R"

  -- Position and velocity of new offspring
  let bpos = physObjectPos bo
      bvel = physObjectVel bo
      ovel = (\(vx,vy) -> (-vx, vy)) bvel

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

ballCollidedWithFire bid = not . null . collisionMask bid ("fire" `isPrefixOf`)

-- A bouncing ball moves freely until there is a collision, then bounces and
-- goes on and on.
--
-- This SF needs an initial position and velocity. Every time
-- there is a bounce, it takes a snapshot of the point of
-- collision and corrected velocity, and starts again.
--
bouncingBall :: Double -> String -> Pos2D -> Vel2D -> ObjectSF
bouncingBall size bid p0 v0 = repeatSF (progressAndBounce size bid) (p0, v0)

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

  let ev = maybeToEvent (changedVelocity bid collisionsWithoutBalls)
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
       startAs v0  = switch (constant v0 &&& restartCond)
                            (\_ -> startAs (0,0))

-- *** Walls

-- | Walls. Each wall has a side and a position.
--
-- NOTE: They are considered game objects instead of having special treatment.
-- The function that turns walls into 'Shape's for collision detection
-- determines how big they really are. In particular, this has implications in
-- ball-through-paper effects (ball going through objects, potentially never
-- coming back), which can be seen if the FPS suddently drops due to CPU load
-- (for instance, if a really major Garbage Collection kicks in.  One potential
-- optimisation is to trigger these with every SF iteration or every rendering,
-- to decrease the workload and thus the likelyhood of BTP effects.
objSideRight  :: ObjectSF
objSideRight  = objWall "rightWall"  RightSide  (gameWidth, 0)

-- | See 'objSideRight'.
objSideLeft   :: ObjectSF
objSideLeft   = objWall "leftWall"   LeftSide   (0, 0)

-- | See 'objSideRight'.
objSideTop    :: ObjectSF
objSideTop    = objWall "topWall"    TopSide    (0, 0)

-- | See 'objSideRight'.
objSideBottom :: ObjectSF
objSideBottom = objWall "bottomWall" BottomSide (0, gameHeight)

-- | Generic wall builder, given a name, a side and its base
-- position.
objWall :: ObjectName -> Side -> Pos2D -> ObjectSF
objWall name side pos = proc (ObjectInput ci cs) -> do
   returnA -< (Object { objectName           = name
                      , objectKind           = Side side
                      , objectPos            = pos
                      , objectVel            = (0,0)
                      , canCauseCollisions   = False
                      , collisionEnergy      = 0
                      })

-- * Auxiliary FRP stuff
maybeToEvent :: Maybe a -> Event a
maybeToEvent = maybe noEvent Event

-- ** ListSF that never dies or produces offspring
inertSF :: SF a b -> ListSF a b
inertSF sf = ListSF (sf >>> arr (\o -> (o, False, [])))

-- ** Event-producing SF combinators
spikeOn :: SF a Bool -> SF a (Event ())
spikeOn sf = noEvent --> (sf >>> edge)

ifDiff :: Eq a => a -> SF a (Event a)
ifDiff x = loopPre x $ arr $ \(x',y') ->
  if x' == y'
   then (noEvent,  x')
   else (Event x', x')

-- ** Repetitive switching

repeatSF :: (c -> SF a (b, Event c)) -> c -> SF a b
repeatSF sf c = switch (sf c) (repeatSF sf)

restartOn :: SF a b -> SF a (Event c) -> SF a b
restartOn sf sfc = switch (sf &&& sfc)
                          (\_ -> restartOn sf sfc)

-- * Objects / collisions auxiliary function
collisionMask :: Eq id
              => id -> (id -> Bool) -> Collisions.Collisions id -> Collisions.Collisions id
collisionMask cId mask = onCollisions ( filter (any (mask . fst))
                                      . filter (any ((== cId).fst))
                                      )

 where onCollisions :: ([[(id, Vel2D)]]         -> [[(id, Vel2D)]])
                    -> Collisions.Collisions id -> Collisions.Collisions id
       onCollisions f = map Collision . f . map collisionData

