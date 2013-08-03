module Point (Point, newPoint, cw, distSq) where

newtype  Point a = Point { runPoint :: (a, a) }

-- The norm is not to export the constructor, but rather the 'factory' 
newPoint x y = Point (x, y)

-- print a point as (x, y)
instance (Show a) => Show (Point a) where
         show (Point(x, y)) = "(" ++ (show x) ++ "," ++ (show y) ++ ")"

instance (Eq a) => Eq (Point a) where
         (==) (Point (x, y)) (Point (x', y')) = x == x' && y == y'
 
instance (Ord a) => Ord (Point a) where
         compare (Point(x, y)) (Point (x', y')) 
                 | cmp_x == EQ = compare y  y'
                 | otherwise = cmp_x
                    where cmp_x = compare x x'
                
-- adding two points
(<+>) :: (Num a) => Point a -> Point a -> Point a 
(<+>) (Point(x, y)) (Point(x', y')) = Point(x + x', y + y')

-- subtracting two points
(<->) :: (Num a) => Point a -> Point a -> Point a 
(<->) (Point(x, y)) (Point(x', y')) = Point(x - x', y - y')

-- multiplication by a scalar
(<*>) :: (Num a) => Point a -> a -> Point a 
(<*>) (Point(x, y)) k = Point(x*k, y*k)

-- dot product
(<.>) :: (Num a) => Point a -> Point a -> a
(<.>) (Point(x, y)) (Point(x', y')) = x*x' + y*y'

-- cross product
(><) :: (Num a) => Point a -> Point a -> a 
(><) (Point(x, y)) (Point(x', y')) = x*y' - x'*y

-- Finds the orientation of vectors (p-r) and (q-r)
-- EQ - collinear
-- LT - pr is less 'clock-wise' than qr
-- GT - pr is more 'clock-wise' than qr 
cw :: (Num a, Ord a) => Point a -> Point a -> Point a -> Ordering
cw p q r
    | x == zero = EQ
    | x < zero = LT
    | x > zero = GT
        where x = (p <-> r) >< (q <-> r)
              zero = fromInteger 0

-- Squared distance between two points
distSq :: (Num a) => Point a -> Point a -> a
distSq (Point (x, y)) (Point (x', y')) = (x - x')^2 + (y - y')^2

