module Hip.Generator where

import Hip.PointSpace
import Hip.ColorSpace
import Hip.Image
--import Hip.Lift


-- | Generates a checkerboard function, defined everywhere
-- cellDim: width and height of each cell of the checkerboard
genCheckerboard :: (Int, Int) -> ColorRGBA -> ColorRGBA -> ImageTree2d ColorRGBA
genCheckerboard cellDim color1 color2 = Leaf checkerFunc
                
                where 
                (xDim, yDim) = cellDim

                checkerFunc :: Point2d -> ColorRGBA
                checkerFunc (Point2d x y) 
                            | isRegion1 = color1
                            | otherwise = color2
                            where
                            xInRange = even $ (floor x) `div` xDim
                            yInRange = even $ (floor y) `div` yDim
                            isRegion1 = (xInRange && yInRange)
                                      || (not xInRange && not yInRange)
