import Control.Applicative
import Data.IORef
import FRP.Yampa as Yampa

import Game
import Display
import Input
import Graphics.UI.Extra.SDL

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
                             return (controllerExit c)
             )
             (wholeGame &&& arr id)

senseTime :: IORef Int -> Controller -> IO DTime
senseTime timeRef = \mInput ->
  let tt  = if controllerSlow      mInput then (/10)  else id
      tt1 = if controllerSuperSlow mInput then (/100) else tt
      tt2 = if controllerFast      mInput then (*10)  else tt1
  in (tt2 . milisecsToSecs) <$> senseTimeRef timeRef
