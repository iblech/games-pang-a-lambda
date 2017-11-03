-- | Auxiliary operations related to monadic computations.
module Control.Extra.Monad where

import Control.Monad

-- | Loop a computation while a condition holds.
whileLoopM :: Monad m => m a -> (a -> Bool) -> (a -> m ()) -> m ()
whileLoopM val cond act = do
  v <- val
  when (cond v) $ do
    act v
    whileLoopM val cond act

-- | Fold using a computation while a condition holds.
foldLoopM :: Monad m => a -> m b -> (b -> Bool) -> (a -> b -> m a) -> m a
foldLoopM val sense cond act = do
  s <- sense
  if cond s
    then do
      val' <- act val s
      foldLoopM val' sense cond act
    else return val
