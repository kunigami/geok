module ConvexHull (convexHull, grahamScan) where

import Point
import Data.List

radialCmp :: (Num a, Ord a) => Point a -> Point a -> Point a  -> Ordering
radialCmp pivot p q 
           | res /= EQ  = res 
           | otherwise = compare (distSq p pivot) (distSq q pivot)  
              where res = cw p q pivot

grahamScan :: (Num a, Ord a) => [Point a] -> [Point a]
grahamScan xs = let (pivot:xs') = sort xs
                    star = sortBy (radialCmp pivot) xs'
                    (dup:h) = grahamScanIt [pivot] (star ++ [pivot])
                      in reverse h

grahamScanIt :: (Num a, Ord a) => [Point a] -> [Point a] -> [Point a]
grahamScanIt (p:p':h) (x:xs) 
             | res == LT = grahamScanIt (x:p:p':h) xs
             | otherwise = grahamScanIt (p':h) (x:xs)
               where res = cw p x p'
-- If the hull has only one point, automatically add the candidate point
grahamScanIt (p:h) (x:xs) = grahamScanIt  (x:p:h) xs
grahamScanIt h [] = h

-- Default implementation of convex hull
convexHull = grahamScan