module Hip.Transform where

import Hip.PointSpace

translate :: (Point t) => t -> t -> t
translate dV = pAdd $ pNeg dV

scale :: (Point t) => Double -> t -> t
scale dS = sMult (1/dS)

rotate :: Double -> Point2d -> Point2d
rotate th (Point2d x y) = Point2d (x * cos th - y * sin th) (y * cos th + x * sin th)
