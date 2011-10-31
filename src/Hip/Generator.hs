module Hip.Generator where

import Hip.ColorSpace
import Hip.Image


-- | Generates an image function that returns a constant color
genConstantColor :: RGBAColor -> Image
genConstantColor color = colorFunc
                 where
                 colorFunc :: Image
                 colorFunc _ = color


-- | Generates a checkerboard function, defined everywhere
-- cellDim: width and height of each cell of the checkerboard
genCheckerboard :: (Int, Int) -> RGBAColor -> RGBAColor -> Image
genCheckerboard cellDim color1 color2 = checkerFunc
                
                where 
                (xDim, yDim) = cellDim

                checkerFunc :: Image
                checkerFunc (x, y) 
                            | isRegion1 = color1
                            | otherwise = color2
                            where
                            xInRange = even $ x `div` xDim
                            yInRange = even $ y `div` yDim
                            isRegion1 = (xInRange && yInRange)
                                      || (not xInRange && not yInRange)
