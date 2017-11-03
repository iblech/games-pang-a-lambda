-- Copyright (c) 2011 - All rights reserved - Keera Studios
module Physics.Shapes where

import Physics.TwoDimensions.Dimensions

-- * Shapes
type AABB   = (Pos2D, Size2D) -- We consider these half the size
type Circle = (Pos2D, Double)
