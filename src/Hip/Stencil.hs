{-# LANGUAGE FlexibleContexts #-} 

module Hip.Stencil where

import Hip.PointSpace
import Hip.ColorSpace
import Hip.Image
import Hip.Transform


gauss3x3, gauss5x5, gauss7x7, gauss9x9 :: ImageRGBA
gauss3x3 = outerProd [1, 2, 1]
gauss5x5 = outerProd [1, 4, 6, 4, 1]
gauss7x7 = outerProd [1, 6, 15, 20, 15, 6, 1]
gauss9x9 = outerProd [1, 8, 28, 56, 70, 56, 28, 8, 1]


-- | returns a 2d kernel function from a 1d kernel (by taking the 
--   outer product with itself
outerProd :: [Double] -> Point2d -> ColorRGBA
outerProd kern1d (Point2d x y) | x < 0 || y < 0 = colorEmpty
                        | ix >= length kern1d || iy >= length kern1d = colorEmpty
                        | otherwise = ColorRGBA cc cc cc cc
                        where 
                        ix = round x
                        iy = round y
                        cc = (kern1d !! ix * kern1d !! iy) / totalsq
                        totalsq = sum kern1d ** 2


convolve :: ImageSYM (Point2d -> t) => (Point2d -> t) -> (Point2d -> t) -> BBox2d -> Point2d -> t
convolve kern img bbox pt = reduce bbox cAdd (binary cMult kern (spatial shift img)) pt
         where
         -- integer dims of the bounding box
         (xdim, ydim) = bboxIntDims bbox

         -- get the kernel's center point
         (cx, cy) = (xdim `div` 2, ydim `div` 2)
         kernCtr = Point2d (fromIntegral cx) (fromIntegral cy)

         shift = translate $ pAdd (pNeg pt) kernCtr
