import ConvexHull
import Point
import Test.HUnit

triangle_in = [(newPoint 1 0), (newPoint 0 0), (newPoint 0 1)]
triangle_out = [(newPoint 0 0), (newPoint 0 1), (newPoint 1 0)]

testTriangle = TestCase (assertEqual "The convex hull of a triangle should be a triangle" triangle_out (convexHull triangle_in))

tests = TestList [TestLabel "test Triangle" testTriangle]
