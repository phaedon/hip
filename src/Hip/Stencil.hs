{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-} 
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
convolve kern img bbox pt = reduce bbox cAdd (binary cMult kern shiftedImg) pt
         where
         (xdim, ydim) = bboxIntDims bbox
         (cx, cy) = (xdim `div` 2, ydim `div` 2)
         shift = translate $ pAdd (pNeg pt) (Point2d (fromIntegral cx) (fromIntegral cy))
         -- TODO: add adjustment for centering the kernel
         shiftedImg = spatial shift img



{-                        
-- | Convolution!
convolveOld :: Stencil -> ImageRGBA -> ImageRGBA
convolveOld kern2 img1 (Point2d px py) = reduceFn kern2 combinedList
         where 
         (xdim, ydim) = dim kern2
         (cx, cy) = (xdim `div` 2, ydim `div` 2)
         coordList = [ (kx, ky) | kx <- [0..xdim-1], ky <- [0..ydim - 1] ]
         translatedImg = img1 . translate (Point2d (-px + cx) (-py + cy))
         combinedList = [ pf (k2 (kx, ky)) (translatedImg (kx, ky)) | (kx, ky) <- coordList ]
         pf = ptwiseFn kern2
         k2 = kernel kern2
-}