-- | Auxiliary operations related to 'VectorSpace'.
module Data.Extra.VectorSpace where

import FRP.Yampa.VectorSpace

-- | Limits a vector by a given norm. If the norm is bigger, the vector is
--   reduced to the given norm keeping its direction.
--
--   For example:
--
--   @limitNorm (8, 6) 5 = (4, 3)@
limitNorm :: (Ord s, VectorSpace v s) => v -> s -> v
limitNorm v mn = if norm v > mn then mn *^ normalize v else v
