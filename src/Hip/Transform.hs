-- | Spatial transforms module.
module Hip.Transform (translate, scale, rotate) where

import Hip.PointSpace

-- | Translation
translate :: (Point t) => t -> t -> t
translate dV = pAdd $ pNeg dV

-- | Scaling
scale :: (Point t) => Double -> t -> t
scale dS = sMult (1/dS)

-- | Rotation (currently implemented only for Point2d)
rotate :: Double -> Point2d -> Point2d
rotate th (Point2d x y) 
       = Point2d (x * cos th - y * sin th) (y * cos th + x * sin th)
