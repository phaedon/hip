module Hip.Generator where

import Hip.PointSpace
import Hip.ColorSpace
import Hip.Image
--import Hip.Lift

-- | Generates an image function that returns a constant color
genConstantColor :: ColorRGBA8 -> ImageRGBA
genConstantColor color = (\_ -> toColorRGBA color)

-- TODO: really want to do this:
--genConstantColor :: (Color c, Image i) => c -> i
--genConstantColor color = (\_ -> color)

-- | Generates a checkerboard function, defined everywhere
-- cellDim: width and height of each cell of the checkerboard
genCheckerboard :: (Int, Int) -> ColorRGBA -> ColorRGBA -> ImageRGBA
genCheckerboard cellDim color1 color2 = checkerFunc
                
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
