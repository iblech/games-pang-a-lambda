module Resources where

import qualified Graphics.UI.SDL           as SDL
import qualified Graphics.UI.SDL.TTF       as TTF

-- import Game.Audio

data Resources = Resources
  { resFont      :: TTF.Font
  , miniFont     :: TTF.Font
  , playerImages :: [(PlayerVisualState, (Int, [SDL.Surface]))]
  , ballImages   :: [(Int, [SDL.Surface])]
  , blockImage   :: SDL.Surface
  , backgrounds  :: [SDL.Surface]
  }

data PlayerVisualState = PlayerVisualStand
                       | PlayerVisualLeft
                       | PlayerVisualRight
                       | PlayerVisualShoot
                       | PlayerVisualHit
 deriving (Eq, Show)

