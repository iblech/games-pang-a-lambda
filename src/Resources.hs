module Resources where

import qualified Graphics.UI.SDL           as SDL
import qualified Graphics.UI.SDL.TTF       as TTF

-- import Game.Audio

data Resources = Resources
  { resFont       :: TTF.Font
  , miniFont      :: TTF.Font
  , standingImage :: [SDL.Surface]
  , rightImage    :: [SDL.Surface]
  , leftImage     :: [SDL.Surface]
  , shootingImage :: [SDL.Surface]
  , hitImage      :: [SDL.Surface]
  }
