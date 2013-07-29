newtype  Point a = Point { runPoint :: (a, a) }

data Orientation = Clockwise | CounterClockwise | Parallel
     deriving (Show)

-- print a point
instance (Show a) => Show (Point a) where
         show (Point(x, y)) = "(" ++ (show x) ++ "," ++ (show y) ++ ")"
                
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

-- TODO: generalize to Num types 
-- Finds the orientation of vectors (p-r) and (q-r)
ccw :: Point Integer -> Point Integer -> Point Integer -> Orientation
ccw p q r
    | x == 0 = Parallel
    | x < 0 = Clockwise
    | x > 0 = CounterClockwise
        where x = (p <-> r) >< (q <-> r)


