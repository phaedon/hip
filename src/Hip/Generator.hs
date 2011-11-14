-- | Generates procedural images
module Hip.Generator (genCheckerboard, testChecker) where

import Hip.PointSpace
import Hip.ColorSpace


-- | Generates a checkerboard function, defined everywhere
-- cellDim: width and height of each cell of the checkerboard
genCheckerboard :: (Int, Int) -- ^ cell dimensions
                   -> ColorRGBA -> ColorRGBA 
                   -> Point2d -> ColorRGBA
genCheckerboard cellDim color1 color2 = checkerFunc
                
                where 
                (xDim, yDim) = cellDim

                checkerFunc :: Point2d -> ColorRGBA
                checkerFunc (Point2d x y) 
                            | isRegion1 = color1
                            | otherwise = color2
                            where
                            xInRange = even $ floor x `div` xDim
                            yInRange = even $ floor y `div` yDim
                            isRegion1 = (xInRange && yInRange)
                                      || (not xInRange && not yInRange)


-- | A prefab B&W checkerboard with 10x10 cells.
-- Convenient for testing
testChecker :: Point2d -> ColorRGBA
testChecker = genCheckerboard (10, 10) colorBlack colorWhite