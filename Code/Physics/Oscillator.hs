{-# LANGUAGE Arrows #-}
module Physics.Oscillator where

import FRP.Yampa

-- * Physics

-- Thanks to Manuel Bärenz for this SF:
-- osci x0 amp period = loopPre (x0 + amp) $ proc ((), x) -> do
--   let acc = - (2.0*pi/period)^(2 :: Int) * (x - x0)
--   v  <- integral -< acc
--   pd <- integral -< v
--   let x' = x0 + amp + pd
--   returnA -< trace (show (acc, v, x, pd)) (x', x')

-- Alternative implementation using rec that I think Henrik
-- will like much more.
--
-- Assumptions:
--    mass             = 1 unit
--    initial velocity = 0
osci amp period = proc _ -> do
  rec
   let acc = - (2.0*pi/period)^(2 :: Int) * p
   v <-             integral -< acc
   p <- (amp +) ^<< integral -< v
  returnA -< p
