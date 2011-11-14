-- | Describes the geometric domains for images in Hip.
module Hip.PointSpace (

       Point (pAdd, pMult, sMult, lerp, pNeg),

       Point2d (Point2d),
       
       pointOrigin,
       roundPoint,
       createPoint       

) where


-- | We use Double for coordinates so that non-integer transforms
--   and coordinate accesses are feasible. 
data Point2d = Point2d !Double !Double
     deriving (Show, Eq)

-- | A Point2d is, of course, a Point:
instance Point Point2d where
         pAdd (Point2d x1 y1) (Point2d x2 y2) = Point2d (x1 + x2) (y1 + y2)
         pMult (Point2d sx sy) (Point2d x y) = Point2d (sx * x) (sy * y)
         s `sMult` Point2d x y = Point2d (s * x) (s * y)
         lerp s p1 p2 = pAdd (sMult s p1) (sMult (1 - s) p2)
         pNeg (Point2d x y) = Point2d (-x) (-y)


-- | Generic Point typeclass.
class Point t where

      -- | Addition (wtd averages, translations, etc)
      pAdd :: t -> t -> t

      -- | Pairwise multiplication (i.e. for rotations)
      pMult :: t -> t -> t
      
      -- | Scalar multiplication (i.e. for taking weighted averages)
      sMult :: Double -> t -> t

      -- | Linear interp
      lerp :: Double -> t -> t -> t

      -- | Change the sign (useful for inverse translations)
      pNeg :: t -> t


-- | 2d integer coordinates
data Point2dInt = Point2dInt !Int !Int
     deriving (Show, Eq)

-- | A handy name for point (0, 0)
pointOrigin :: Point2d
pointOrigin = Point2d 0 0

-- | Converts a Point2d to a pair of integers
roundPoint :: Point2d -> (Int, Int)
roundPoint (Point2d x y) = (round x, round y)

-- | Converts a pair of integers to a Point2d
createPoint :: (Int, Int) -> Point2d
createPoint (x, y) = Point2d (fromIntegral x) (fromIntegral y)