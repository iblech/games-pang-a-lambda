-- | Auxiliary operations related to comparable numbers.
module Data.Extra.Ord where

-- | Adjust a number to a range. If the number is within range bounds, it
--   is returned unchanged. Otherwise, the upper or lower boundary is
--   returned.
--
--   @inRange (mn,mx) a = min mx (max mn a)@
inRange :: Ord a => (a, a) -> a -> a
inRange (mn, mx) n | n < mn    = mn
                   | n > mx    = mx
                   | otherwise = n
