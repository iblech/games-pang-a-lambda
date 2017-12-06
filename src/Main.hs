import Data.IORef
import FRP.Yampa as Yampa
import System.Mem
import Game.Clock

import Game.Display
import Game.GamePlay
import Game.Input

main :: IO ()
main = do

  initializeDisplay

  timeRef       <- initializeTimeRef
  controllerRef <- initializeInputDevices
  res           <- loadResources

  initGraphs res
  reactimate (senseInput controllerRef)
             (\_ -> do
                -- Get clock and new input
                mInput <- senseInput controllerRef
                dtSecs <- senseTime timeRef mInput
                -- trace ("Time : " ++ printf "%.5f" dtSecs) $
                return (if controllerPause mInput then 0 else dtSecs, Just mInput)
             )
             (\_ (e,c) -> do render res e
                             performGC
                             return (controllerExit c)
             )
             (wholeGame &&& arr id)

senseTime :: IORef Int -> Controller -> IO DTime
senseTime timeRef = \mInput ->
  let tt  = if controllerSlow      mInput then (/10)      else id
      tt1 = if controllerSuperSlow mInput then (/100)     else tt
      tt2 = if controllerFast      mInput then (*10)      else tt1
      tt3 = id -- if controllerReverse   mInput then (\x -> -x) else id
  in (tt3 . tt2 . milisecsToSecs) <$> senseTimeRef timeRef

