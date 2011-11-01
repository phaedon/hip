module Hip.PointSpace where

-- | First iteration. To be deprecated later.
type Point1D = Int
type Point2D = (Int, Int)
type Point3D = (Int, Int, Int)


-- | We use Double for coordinates so that non-integer transforms
--   and coordinate accesses are feasible. (Will be helpful for
--   antialiasing later on.)
newtype Point2d = Point2d (Double, Double)
        deriving (Show, Eq)

instance Point Point2d where
         f ** Point2d (x, y) = Point2d (f * x, f * y)
         
-- | Generic Point typeclass.
class Point a where
      -- Scalar multiplication (for taking weighted averages)
      (**) :: Double -> a -> a

