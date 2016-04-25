module Data.Extra.Ord where

inRange :: Ord a => (a, a) -> a -> a
inRange (mn, mx) n | n < mn    = mn
                   | n > mx    = mx
                   | otherwise = n
