-- | A very simple physics subsytem. It currently detects shape
-- overlaps only, the actual physics movement is carried out
-- in Yampa itself, as it is very simple using integrals and
-- derivatives.
module Physics.TwoDimensions.Shapes where

import FRP.Yampa.VectorSpace
import Physics.TwoDimensions.Dimensions

import Game.Constants

import Physics.Collisions

-- | Side of a rectangle
data Side = TopSide | BottomSide | LeftSide | RightSide
  deriving (Eq,Show)

-- | Opposite side
--
-- If A collides with B, the collision sides on
-- A and B are always opposite.
oppositeSide :: Side -> Side
oppositeSide TopSide    = BottomSide
oppositeSide BottomSide = TopSide
oppositeSide LeftSide   = RightSide
oppositeSide RightSide  = LeftSide

data Shape = Rectangle  Pos2D Size2D -- A corner and the whole size
           | Circle     Pos2D Double -- Position and radius
           | SemiPlane  Pos2D Side   --

-- | Detects if two shapes overlap.
--
-- Rectangles: overlap if projections on both axis overlap,
-- which happens if x distance between centers is less than the sum
-- of half the widths, and the analogous for y and the heights.

overlapShape :: Shape -> Shape -> Bool
overlapShape (Circle p1 s1) (Circle p2 s2) = (dist - (s1 + s2)) < sigma
  where (dx, dy) = p2 ^-^ p1
        dist     = sqrt (dx**2 + dy**2)
        sigma    = 1
overlapShape (Circle (p1x,p1y) s1) (SemiPlane (px,py) side) = case side of
  LeftSide   -> p1x - s1 <= px
  RightSide  -> p1x + s1 >= px
  TopSide    -> p1y - s1 <= py
  BottomSide -> p1y + s1 >= py
overlapShape s@(SemiPlane _ _) c@(Circle _ _) = overlapShape c s
overlapShape r@(Rectangle _ _) c@(Circle _ _) = overlapShape c r
overlapShape (Circle p1 s1)    (Rectangle p2 s2) =
  circleAABBOverlap (p1,s1) (rectangleToCentre (p2,s2))
overlapShape (Rectangle p1 s1) (Rectangle p2 s2) =
  overlapsAABB2 (rectangleToCentre (p1, s1)) (rectangleToCentre (p2, s2))
overlapShape (Rectangle p1 s1) (SemiPlane p2 side2) =
  let (p2', s2') = semiplaneRectangle p2 side2
  in overlapsAABB2 (rectangleToCentre (p1, s1)) (rectangleToCentre (p2', s2'))
overlapShape p@(SemiPlane _ _) r@(Rectangle _ _) = overlapShape r p
overlapShape _                 _                 = False -- Not really, it's just that we don't care

semiplaneRectangle :: Pos2D -> Side -> (Pos2D, (Double, Double))
semiplaneRectangle p2 s2 =  case s2 of
  LeftSide   -> (p2 ^-^ (100, 100), (100,   height + 200))
  RightSide  -> (p2 ^-^ (0,   100), (100,   height + 200))
  TopSide    -> (p2 ^-^ (100, 100), (width + 200, 100))
  BottomSide -> (p2 ^-^ (100,   0), (width + 200, 100))

rectangleToCentre ((px, py), (sw, sh)) = ((px + (sw / 2), py + (sh / 2)), (sw / 2, sh / 2))
