-- | Auxiliary operations related to numbers.
module Data.Extra.Num where

-- | Ensure that a number is positive. Negates it if
--   negative.
--
--   Same as abs, except that it does not change the number
--   if it is already positive.
--
--   TODO: any difference in speed?
ensurePos :: (Eq a, Num a) => a -> a
ensurePos e = if signum e == (-1) then negate e else e

-- | Ensure that a number is negative. Negates it if
--   positive.
--
--   Same as @(negate . abs)@, except that it does not change the number
--   if it is already negative.
--
--   TODO: any difference in speed?
ensureNeg :: (Eq a, Num a) => a -> a
ensureNeg e = if signum e == 1 then negate e else e

-- | Class to capture margins of error for different number formats.
class Similar a where
  -- | Margin of error. Any number smaller will be considered negligible.
  sigma :: a -- margin of error

-- | For the purposes of this game, differences below 0.01 are considered
--   negligible.
instance Similar Float where
  sigma = 0.01

-- | For the purposes of this game, differences below 0.01 are considered
--   negligible.
instance Similar Double where
  sigma = 0.01

-- | Similarity relation. Two numbers are similar if their distance is
--   smaller than the margin of error for the type.
(=~) :: (Num a, Ord a, Similar a) => a -> a -> Bool
x =~ y = abs (x - y) < sigma

