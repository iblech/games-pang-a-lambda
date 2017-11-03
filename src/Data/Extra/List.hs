-- | Auxiliary List functions
module Data.Extra.List where

-- | Update an element in a list.
updateL :: [a] -> Int -> a -> [a]
updateL []     _ x  = [x]
updateL (_:as) 0 a  = a : as
updateL (a:as) n a' = a : updateL as (n-1) a'
