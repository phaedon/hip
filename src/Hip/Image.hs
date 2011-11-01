module Hip.Image where

import Hip.ColorSpace
import Hip.PointSpace


type Image = Point2D -> RGBAColor
type FloatImage = Point2D -> Float
type DoubleImage = Point2D -> Double
type BinaryImage = Point2D -> Bool

type Kernel2D = FloatImage

--type ImageGen a b = (Point a, Color b) => a -> b
