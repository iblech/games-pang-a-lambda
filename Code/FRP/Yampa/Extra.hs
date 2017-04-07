module FRP.Yampa.Extra where

import FRP.Yampa
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

restartOn :: SF a b -> SF a (Event c) -> SF a b
restartOn sf sfc = switch (sf &&& sfc)
                          (\_ -> restartOn sf sfc)
