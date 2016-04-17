-- Copyright (c) 2011 - All rights reserved - Keera Studios
module Shapes where

import Data

-- * Shapes
type AABB   = (Pos2D, Size2D) -- We consider these half the size
type Circle = (Pos2D, Double)
