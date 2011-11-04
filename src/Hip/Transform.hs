module Hip.Transform where

import Hip.PointSpace

translate :: (Point t) => t -> t -> t
translate dV v = pAdd (pNeg dV) v

scale :: (Point t) => Double -> t -> t
scale dS p = sMult (1/dS) p

rotate :: Double -> Point2d -> Point2d
rotate th (Point2d x y) = Point2d (x * cos th - y * sin th) (y * cos th + x * sin th)
