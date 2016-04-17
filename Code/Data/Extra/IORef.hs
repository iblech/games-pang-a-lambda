
module Data.Extra.IORef where

import Data.IORef

-- * Aux IOREf
modifyIORefIO :: IORef a -> (a -> IO a) -> IO a
modifyIORefIO ref modify = do
  v <- readIORef ref
  new <- modify v
  writeIORef ref new
  return new
