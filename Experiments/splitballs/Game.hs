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
import Data.Extra.VectorSpace
import Physics.TwoDimensions.Collisions
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
   proc (i) -> do
      -- Adapt Input
      let oi = (uncurry ObjectInput) i

      -- Step
      -- Each obj processes its movement forward
      ol  <- dlSwitch objs -< oi
      let cs' = detectCollisions ol

      -- Output
      tLeft <- time        -< ()
      returnA -< ((ol, tLeft), cs')

-- * Game objects
--
-- | Objects initially present: the walls, the ball, the paddle and the blocks.
initialObjects :: [ListSF ObjectInput Object]
initialObjects =
    [ inertSF objSideRight
    , inertSF objSideTop
    , inertSF objSideLeft
    , inertSF objSideBottom
    ]
    ++ objEnemies

-- *** Enemy
objEnemies :: [ListSF ObjectInput Object]
objEnemies =
  [ splittingBall ballWidth "enemy1" (300, 300) (360, -350)
  , splittingBall ballWidth "enemy2" (500, 300) (-330, -280)
  , splittingBall ballWidth "enemy3" (200, 100) (-300, -250)
  , splittingBall ballWidth "enemy4" (100, 200) (-200, -150)
  ]

splittingBall :: Double -> String -> Pos2D -> Vel2D -> ListSF ObjectInput Object
splittingBall size bid p0 v0 = ListSF $ proc i -> do
  t         <- localTime                    -< ()
  bo        <- bouncingBall size bid p0 v0 -< i
  click     <- edge -< controllerClick (userInput i)
  let tooSmall    = size <= (ballWidth / 2)
      shouldSplit = isEvent click

  let offspringSize = (5 * size / 6) -- Or: size

  let offspringID = (bid ++ show t) -- Should be unique or collisions won't work

      offspring = [ splittingBall offspringSize offspringID bpos (0,0)
                  | shouldSplit && not tooSmall
                  , let bpos = physObjectPos bo
                  ]

      dead = shouldSplit && tooSmall

  returnA -< (bo, dead, offspring)

-- *** Ball

-- A bouncing ball moves freely until there is a collision, then bounces and
-- goes on and on.
--
-- This SF needs an initial position and velocity. Every time
-- there is a bounce, it takes a snapshot of the point of
-- collision and corrected velocity, and starts again.
--
bouncingBall :: Double -> String -> Pos2D -> Vel2D -> ObjectSF
bouncingBall size bid p0 v0 =
  switch progressAndBounce
         (uncurry (bouncingBall size bid)) -- Somehow it would be clearer like this:
                                           -- \(p', v') -> bouncingBall p' v')
 where

       -- Calculate the future tentative position, and
       -- bounce if necessary.
       --
       -- The ballBounce needs the ball SF' input (which has knowledge of
       -- collisions), so we carry it parallely to the tentative new positions,
       -- and then use it to detect when it's time to bounce

       --      ==========================    ============================
       --     -==--------------------->==--->==-   ------------------->==
       --    / ==                      ==    == \ /                    ==
       --  --  ==                      ==    ==  X                     ==
       --    \ ==                      ==    == / \                    ==
       --     -==----> freeBall' ----->==--->==--------> ballBounce -->==
       --      ==========================    ============================
       progressAndBounce = (arr id &&& freeBall') >>> (arr snd &&& ballBounce bid)

       -- Position of the ball, starting from p0 with velicity v0, since the
       -- time of last switching (or being fired, whatever happened last)
       -- provided that no obstacles are encountered.
       freeBall' = freeBall size bid p0 v0

-- | Detect if the ball must bounce and, if so, take a snapshot of the object's
-- current position and velocity.
--
-- NOTE: To avoid infinite loops when switching, the initial input is discarded
-- and never causes a bounce. This works in this game and in this particular
-- case because the ball never-ever bounces immediately as fired from the
-- paddle.  This might not be true if a block is extremely close, if you add
-- flying enemies to the game, etc.
ballBounce :: String -> SF (ObjectInput, Object) (Event (Pos2D, Vel2D))
ballBounce bid = noEvent --> ballBounce' bid

-- | Detect if the ball must bounce and, if so, take a snapshot of the object's
-- current position and velocity.
--
-- This does the core of the work, and does not ignore the initial input.
--
-- It proceeds by detecting whether any collision affects
-- the ball's velocity, and outputs a snapshot of the object
-- position and the corrected velocity if necessary.
ballBounce' :: String -> SF (ObjectInput, Object) (Event (Pos2D, Vel2D))
ballBounce' bid = proc (ObjectInput ci cs, o) -> do
  -- HN 2014-09-07: With the present strategy, need to be able to
  -- detect an event directly after
  -- ev <- edgeJust -< changedVelocity "ball" cs
  let ev = maybeToEvent (changedVelocity bid cs)
  returnA -< fmap (\v -> (objectPos o, v)) ev

-- | Position of the ball, starting from p0 with velicity v0, since the time of
-- last switching (that is, collision, or the beginning of time --being fired
-- from the paddle-- if never switched before), provided that no obstacles are
-- encountered.
freeBall :: Double -> String -> Pos2D -> Vel2D -> ObjectSF
freeBall size name p0 v0 = proc (ObjectInput ci cs) -> do

  -- Cap speed
  -- let v = limitNorm v0 maxVNorm
  initV <- startAs v0 -< ci

  curV <- vdiffSF -< (initV, (0, -100.8), ci)

  -- vdiff <- integral -< (0, -100.8)
  -- let v' = v0 ^+^ vdiff
  --     v = limitNorm v' maxVNorm
  --     curV = if controllerStop ci then (0,0) else v

  -- Any free moving object behaves like this (but with
  -- acceleration. This should be in some FRP.NewtonianPhysics
  -- module)
  p <- (p0 ^+^) ^<< integral -< curV

  let obj = Object { objectName           = name
                   , objectKind           = Ball size
                   , objectPos            = p
                   , objectVel            = curV
                   , canCauseCollisions   = True
                   , collisionEnergy      = 1
                   }

  returnA -< obj
 where adjustV vd  = limitNorm (v0 ^+^ vd) maxVNorm
       restartCond = noEvent --> (arr snd >>> arr controllerStop >>> edge)
       vdiffSF     = proc (iv, acc, ci) -> do
                       vd <- restartOn (arr fst >>> integral) restartCond  -< (acc, ci)
                       v <- arr (uncurry (^+^)) -< (iv, vd)
                       returnA -< limitNorm v maxVNorm
       startAs v0  = switch (constant v0 &&& (noEvent --> (arr controllerStop >>> edge)))
                            (\_ -> startAs (0,0))

restartOn :: SF a b -> SF a (Event c) -> SF a b
restartOn sf sfc = switch (sf &&& sfc)
                          (\_ -> restartOn sf sfc)

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

inertSF :: SF a b -> ListSF a b
inertSF sf = ListSF (sf >>> arr (\o -> (o, False, [])))
