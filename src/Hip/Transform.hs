module Hip.Transform where

import Hip.PointSpace

type Transform = Point2D -> Point2D

translate :: Point2D -> Transform
translate (dx, dy) (x, y) = (x - dx, y - dy)

scale :: Point2D -> Transform
scale (sx, sy) (x, y) = outpt
      where 
      adjx, adjy:: Double
      adjx = fromIntegral x / fromIntegral sx
      adjy = fromIntegral y / fromIntegral sy
      outpt = (round adjx, round adjy)

rotate :: Float -> Transform
rotate th (x, y) = (round rawx, round rawy)
       
       where
       fx = fromIntegral x
       fy = fromIntegral y
       (rawx, rawy) = (fx * cos th - fy * sin th,
                       fy * cos th + fx * sin th)                      