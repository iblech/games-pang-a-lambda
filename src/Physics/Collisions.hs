-- Copyright (c) 2011 - All rights reserved - Keera Studios
module Physics.Collisions where

import Control.Applicative
import Control.Arrow
import Data.Maybe

import FRP.Yampa.VectorSpace
import Physics.TwoDimensions.Dimensions
import Physics.Shapes
import Data ( pointX, rotateRespect, unrotateRespect
            , minimumWith, swap
            )

circleAABBOverlap :: Circle -> AABB -> Bool
circleAABBOverlap c@(cp,cr) (rp, rs) =
  circleAABBOverlap' ((0,0), cr) (rp ^-^ cp, rs)

circleAABBOverlap' :: Circle -> AABB -> Bool
circleAABBOverlap' ((p1x,p1y),r1) (p2@(p2x, p2y), s2@(w2, h2)) =
  overlapX && overlapY && overlapP11 && overlapP12 && overlapP21 && overlapP22
 where -- Square coordinates
       p211@(p211x, p211y) = p2 ^-^ s2
       p212@(p212x, p212y) = p2 ^+^ (-w2, h2)
       p221@(p221x, p221y) = p2 ^+^ (w2, -h2)
       p222@(p222x, p222y) = p2 ^+^ s2

       -- Horizontal projection overlap
       overlapX =  (-r1, r1) `overlapSegment` (p2x - w2, p2x + w2)
       overlapY =  (-r1, r1) `overlapSegment` (p2y - h2, p2y + h2)

       rectangleVertices = [p211, p212, p221, p222]

       rotatedP211 = map (fst.rotateRespect p211) rectangleVertices
       rotatedP212 = map (fst.rotateRespect p212) rectangleVertices
       rotatedP221 = map (fst.rotateRespect p221) rectangleVertices
       rotatedP222 = map (fst.rotateRespect p222) rectangleVertices

       projection ps = (minimum ps, maximum ps)

       overlapP11 = (-r1, r1) `overlapSegment` projection rotatedP211
       overlapP12 = (-r1, r1) `overlapSegment` projection rotatedP212
       overlapP21 = (-r1, r1) `overlapSegment` projection rotatedP221
       overlapP22 = (-r1, r1) `overlapSegment` projection rotatedP222

       overlapSegment (x01,x02) (x11, x12) = min x02 x12 > max x01 x11

-- * Colision detection
responseCircleAABB :: Circle -> AABB -> Maybe Pos2D
responseCircleAABB c@(cp,cr) (rp, rs) =
  responseCircleAABB' ((0,0), cr) (rp ^-^ cp, rs)

responseCircleAABB' :: Circle -> AABB -> Maybe Pos2D
responseCircleAABB' ((p1x,p1y),r1) (p2@(p2x, p2y), s2@(w2, h2))
  | all isJust overlaps
  = (Just . snd) $ minimumWith (abs.fst) $ map fromJust overlaps
  | otherwise
  = Nothing

 where overlaps = [ overlapX,   overlapY
                  , overlapP11, overlapP12
                  , overlapP21, overlapP22
                  ]

       -- Square coordinates
       p211@(p211x, p211y) = p2 ^-^ s2
       p212@(p212x, p212y) = p2 ^+^ (-w2, h2)
       p221@(p221x, p221y) = p2 ^+^ (w2, -h2)
       p222@(p222x, p222y) = p2 ^+^ s2

       -- Horizontal projection overlap
       overlapX =  (pointX &&& id)   <$> (-r1, r1) `overlapSegment` (p2x - w2, p2x + w2)
       overlapY =  (pointX &&& swap) <$> (-r1, r1) `overlapSegment` (p2y - h2, p2y + h2)

       rectangleVertices = [p211, p212, p221, p222]

       rotatedP211 = map (rotateRespect p211) rectangleVertices
       rotatedP212 = map (rotateRespect p212) rectangleVertices
       rotatedP221 = map (rotateRespect p221) rectangleVertices
       rotatedP222 = map (rotateRespect p222) rectangleVertices

       xProjection = (minimum &&& maximum) . map pointX

       circleOverlaps = overlapSegment (-r1, r1)

       overlapP11 = (pointX &&& unrotateRespect p211) <$> circleOverlaps (xProjection rotatedP211)
       overlapP12 = (pointX &&& unrotateRespect p212) <$> circleOverlaps (xProjection rotatedP212)
       overlapP21 = (pointX &&& unrotateRespect p221) <$> circleOverlaps (xProjection rotatedP221)
       overlapP22 = (pointX &&& unrotateRespect p222) <$> circleOverlaps (xProjection rotatedP222)

       overlapSegment (x01,x02) (x11, x12)
         | segmentLength' >  0 = Just (segmentLength, 0)
         | otherwise           = Nothing
         where segmentLength   = if x01 < x11 then -segmentLength' else segmentLength'
               segmentLength'  = if segmentLength'' == 0 then 1 else segmentLength''
               segmentLength'' = min x02 x12 - max x01 x11

responseAABB2 :: AABB -> AABB -> Maybe Pos2D
responseAABB2 (pos1, size1) (pos2, size2)
 | overlap && overlapx > overlapy = Just cy
 | overlap && overlapy > overlapx = Just cx
 | overlap                        = Just (cx ^+^ cy)
 | otherwise                      = Nothing
 where (x1,y1) = pos1
       (w1,h1) = size1
       (x2,y2) = pos2
       (w2,h2) = size2
       toRight = x1 > x2
       toLeft  = x1 < x2
       above   = y1 < y2
       below   = y1 > y2
       overlapx = max 0 (w1 + w2 - abs (x1 - x2))
       overlapy = max 0 (h1 + h2 - abs (y1 - y2))
       overlap  = overlapx > 0 && overlapy > 0
       cx = if toRight then (overlapx, 0) else (-overlapx, 0)
       cy = if above then (0, -overlapy) else (0, overlapy)
       (x1,y1) ^+^ (x2,y2) = (x1+x2, y1+y2)

overlapsAABB2 :: AABB -> AABB -> Bool
overlapsAABB2 (pos1, size1) (pos2, size2) =
  abs (x1 - x2) < w1 + w2 && abs (y1 - y2) < h1 + h2
 where (x1,y1) = pos1
       (w1,h1) = size1
       (x2,y2) = pos2
       (w2,h2) = size2
