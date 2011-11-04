module Hip.PointSpace where

-- import qualified Data.Vector.Unboxed as VU

{-
type Point t = VU.Vector t
type IPoint = Point Int
type DPoint = Point Double

pAdd :: (VU.Unbox t, Num t) => Point t -> Point t -> Point t
pAdd a b = VU.zipWith (+) a b

pMult :: (VU.Unbox t, Num t) => Point t -> Point t -> Point t
pMult a b = VU.zipWith (*) a b

sMult :: Double -> DPoint -> DPoint
sMult s pt = VU.map (*s) pt

lerp :: Double -> DPoint -> DPoint -> DPoint
lerp alpha a b = pAdd (sMult alpha a) (sMult (1 - alpha) b)
-}

-- | We use Double for coordinates so that non-integer transforms
--   and coordinate accesses are feasible. (Will be helpful for
--   antialiasing later on.) 

data Point2d = Point2d !Double !Double
     deriving (Show, Eq)

instance Point Point2d where
         pAdd (Point2d x1 y1) (Point2d x2 y2) = Point2d (x1 + y1) (x2 + y2)
         pMult (Point2d sx sy) (Point2d x y) = Point2d (sx * x) (sy * y)
         s `sMult` Point2d x y = Point2d (s * x) (s * y)
         lerp s p1 p2 = pAdd (sMult s p1) (sMult (1 - s) p2)
         pNeg (Point2d x y) = Point2d (-x) (-y)

-- | Generic Point typeclass.

class Point t where

      -- Addition (wtd averages, translations, etc)
      pAdd :: t -> t -> t

      -- Pairwise multiplication (i.e. for rotations)
      pMult :: t -> t -> t

      -- Scalar multiplication (i.e. for taking weighted averages)
      sMult :: Double -> t -> t

      -- Linear interp
      lerp :: Double -> t -> t -> t

      -- Change the sign (useful for inverse translations)
      pNeg :: t -> t


data Point2dInt = Point2dInt !Int !Int
     deriving (Show, Eq)