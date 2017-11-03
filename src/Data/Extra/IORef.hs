-- | Auxiliary operations related to IORefs.
module Data.Extra.IORef where

import Data.IORef

-- | Mutate the contents of an 'IORef' using a monadic computation, and 
--   returns the new value.
--
-- Be warned that 'modifyIORefIO' does not apply the function strictly.  This
-- means if the program calls 'modifyIORefIO' many times, but seldomly uses the
-- value, thunks will pile up in memory resulting in a space leak.  This is a
-- common mistake made when using an IORef as a counter.  For example, the
-- following will likely produce a stack overflow:
--
-- >ref <- newIORef 0
-- >replicateM_ 1000000 $ modifyIORef ref (return . (+1))
-- >readIORef ref >>= print
--
-- To avoid this problem, use 'modifyIORef'' instead.
modifyIORefIO :: IORef a -> (a -> IO a) -> IO a
modifyIORefIO ref modify = do
  v <- readIORef ref
  new <- modify v
  writeIORef ref new
  return new
