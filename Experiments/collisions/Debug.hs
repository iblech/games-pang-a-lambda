module Debug where

import Control.Monad (when, void)

import Constants

debug :: Bool -> String -> IO ()
debug b msg = when b $ putStrLn msg
