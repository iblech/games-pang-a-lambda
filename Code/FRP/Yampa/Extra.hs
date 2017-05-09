{-# LANGUAGE MultiWayIf #-}
module FRP.Yampa.Extra where

import Debug.Trace
import FRP.Yampa
import FRP.Yampa.InternalCore
import FRP.Yampa.Switches

-- * Auxiliary FRP stuff
maybeToEvent :: Maybe a -> Event a
maybeToEvent = maybe noEvent Event

-- ** ListSF that never dies or produces offspring
inertSF :: SF a b -> ListSF a b
inertSF sf = ListSF (sf >>> arr (\o -> (o, False, [])))

-- ** Event-producing SF combinators
spikeOn :: SF a Bool -> SF a (Event ())
spikeOn sf = noEvent --> (sf >>> edge)

ifDiff :: Eq a => a -> SF a (Event a)
ifDiff x = loopPre x $ arr $ \(x',y') ->
  if x' == y'
   then (noEvent,  x')
   else (Event x', x')

-- ** Repetitive switching

repeatSF :: (c -> SF a (b, Event c)) -> c -> SF a b
repeatSF sf c = switch (sf c) (repeatSF sf)

repeatRevSF :: (c -> SF a (b, Event c)) -> c -> SF a b
repeatRevSF sf c = revSwitch (sf c) (repeatRevSF sf)

restartOn :: SF a b -> SF a (Event c) -> SF a b
restartOn sf sfc = switch (sf &&& sfc)
                          (\_ -> restartOn sf sfc)

-- restartRevOn :: SF a b -> SF a (Event c) -> SF a b
-- restartRevOn sf sfc = switch (sf &&& sfc)
--                              (\_ -> restartOn sf sfc)
-- 

revSwitch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
revSwitch (SF {sfTF = tf10}) k = SF {sfTF = tf0}
    where
        tf0 a0 =
            case tf10 a0 of
                (sf1, (b0, NoEvent))  -> (switchAux sf1 k, b0)
                (sf1, (_,  Event c0)) -> switchingPoint sf1 k (sfTF (k c0) a0)

        switchingPoint :: SF' a (b, Event c) -> (c -> SF a b) -> (SF' a b, b) -> (SF' a b, b)
        switchingPoint sf1 k (sfN', b) = (sf', b)
          where sf' = SF' tf'
                tf' dt a = if | dt < 0  -> sfTF' (switchAux sf1 k) dt a
                                           -- let (sf1', b') = sfTF' sf1 dt a
                                           -- in (switchAux sf1' k, b')
                              | dt > 0  -> switchingPoint' sf1 k dt (sfTF' sfN' dt a)
                              | dt == 0 -> switchingPoint sf1 k (sfN', b)

        switchingPoint' :: SF' a (b, Event c) -> (c -> SF a b) -> DTime -> (SF' a b, b) -> (SF' a b, b)
        switchingPoint' sf1 k accumDT (sfN', b) = (sf', b)
          where sf' = SF' tf'
                tf' dt a = let dt' = dt + accumDT
                           in if | dt < 0  -> if | dt' < 0  -> sfTF' (switchAux sf1 k) dt' a
                                                 | dt' > 0  -> dt' `seq` switchingPoint' sf1 k dt' (sfTF' sfN' dt a)
                                                 | dt' == 0 -> switchingPoint' sf1 k accumDT (sfN', b)
                                 | dt > 0  -> dt' `seq` switchingPoint' sf1 k dt' (sfTF' sfN' dt a)
                                 | dt == 0 -> switchingPoint' sf1 k accumDT (sfN', b)


        switchAux :: SF' a (b, Event c) -> (c -> SF a b) -> SF' a b
        switchAux sf1                          k = SF' tf
            where
                tf dt a =
                    case (sfTF' sf1) dt a of
                        (sf1', (b, NoEvent)) -> (switchAux sf1' k, b)
                        (_,    (_, Event c)) -> switchingPoint sf1 k (sfTF (k c) a)

alwaysForward :: SF a b -> SF a b
alwaysForward sf = SF $ \a -> let (sf', b) = sfTF sf a
                              in (alwaysForward' sf', b)

alwaysForward' :: SF' a b -> SF' a b
alwaysForward' sf = SF' $ \dt a -> let (sf', b) = sfTF' sf (max dt (-dt)) a
                                   in (alwaysForward' sf', b)

checkpoint :: SF a (b, Event (), Event ()) -> SF a b
checkpoint sf = SF $ \a -> let (sf', (b, advance, reset)) = sfTF sf a
                           in case reset of
                                Event () -> error "loop"
                                NoEvent -> let pt = case advance of 
                                                      Event () -> Left sf'
                                                      NoEvent  -> Right sf
                                           in (checkpoint' pt sf', b)

checkpoint' :: Either (SF' a (b, Event (), Event ())) (SF a (b, Event (), Event ()))
            -> (SF' a (b, Event (), Event ()))
            -> SF' a b
checkpoint' rstPt sf' = SF' $ \dt a -> let (sf'', (b, advance, reset)) = sfTF' sf' dt a
                                       in case reset of
                                            Event () -> case rstPt of
                                                          Left sf''' -> sfTF' (checkpoint' rstPt sf''') dt a
                                                          Right sf   -> sfTF (checkpoint sf) a
                                            NoEvent -> let pt = case advance of
                                                                  Event () -> Left sf''
                                                                  NoEvent -> rstPt
                                                       in (checkpoint' pt sf'', b) 

forgetPast sf = SF $ \a -> let (sf', b) = sfTF sf a
                           in (forgetPast' 0 sf', b)

forgetPast' time sf' = SF' $ \dt a -> let time' = time + dt
                                      in -- trace (show time') $
                                          if time' < 0
                                           then let (sf'', b) = sfTF' sf' (-time) a
                                                in (forgetPast' 0 sf'', b)
                                           else let (sf'', b) = sfTF' sf' dt a
                                                in (forgetPast' time' sf'', b)

limitHistory :: DTime -> SF a b -> SF a b
limitHistory time sf = SF $ \a -> let (sf', b) = sfTF sf a
                                  in (limitHistory' 0 time sf', b)

limitHistory' :: Time -> DTime -> SF' a b -> SF' a b
limitHistory' curT maxT sf' = SF' $ \dt a -> let curT' = curT + dt
                                                 time' = if curT' > maxT then maxT else curT'
                                             in -- trace (show (dt, curT, maxT, maxMaxT)) $
                                                 if time' < 0
                                                  then let (sf'', b) = sfTF' sf' (-curT) a
                                                       in (limitHistory' 0 maxT sf'', b)
                                                  else let (sf'', b) = sfTF' sf' dt a
                                                       in (limitHistory' time' maxT sf'', b)

clocked :: SF a DTime -> SF a b -> SF a b
clocked clockSF sf = SF $ \a -> let (sf', b)  = sfTF sf a
                                    (cSF', _) = sfTF clockSF a
                                in (clocked' cSF' sf', b)

clocked' :: SF' a DTime -> SF' a b -> SF' a b
clocked' clockSF sf = SF' $ \dt a -> let (cSF', dt') = sfTF' clockSF dt a
                                         (sf', b) = sfTF' sf dt' a
                                     in (clocked' cSF' sf', b)

deltas = localTime >>> loopPre 0 (arr $ \(lt, ot) -> (lt-ot, lt))
