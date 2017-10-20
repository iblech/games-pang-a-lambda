module Game.Debug where

import Control.Monad (when, void)

debug :: Bool -> String -> IO ()
debug b msg = when b $ putStrLn msg
