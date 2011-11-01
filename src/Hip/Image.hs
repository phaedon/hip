module Hip.Image where

import Hip.ColorSpace
import Hip.PointSpace

type Image = Point2D -> RGBAColor
type Kernel2D = Point2D -> Float

--type ImageGen a b = (Point a, Color b) => a -> b
