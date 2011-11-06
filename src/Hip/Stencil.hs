module Hip.Stencil where

import Hip.PointSpace
import Hip.ColorSpace
import Hip.Image
import Hip.Transform


-- | A Stencil is an image with fixed dimensions
data Stencil = Stencil {
     kernel :: ImageRGBA,
     dim :: (Int, Int), -- dimensions
     ptwiseFn :: ColorRGBA -> ColorRGBA -> ColorRGBA, -- this will become more generic
     reduceFn :: [ColorRGBA] -> ColorRGBA  -- and so will this
     }


-- | Combines two points: the first is from the kernel,
--  the second from the image, and outputs a float color in [0,1]
--  i.e. this is pointwise multiplication
--ptwiseConv :: Double -> ColorRGBA -> ColorRGBA
--ptwiseConv = cScale

reduceConv :: [ColorRGBA] -> ColorRGBA
reduceConv = foldr cAdd (ColorRGBA 0 0 0 0)

gauss3x3, gauss5x5, gauss7x7, gauss9x9 :: Stencil
gauss3x3 = Stencil (outerProd [1, 2, 1]) (3, 3) cScale reduceConv
gauss5x5 = Stencil (outerProd [1, 4, 6, 4, 1]) (5, 5) cScale reduceConv
gauss7x7 = Stencil (outerProd [1, 6, 15, 20, 15, 6, 1]) (7, 7) cScale reduceConv
gauss9x9 = Stencil (outerProd [1, 8, 28, 56, 70, 56, 28, 8, 1]) (9, 9) cScale reduceConv


-- | returns a 2d kernel function from a 1d kernel (by taking the 
--   outer product with itself
outerProd :: [Double] -> Point2d -> ColorRGBA
outerProd kern1d (Point2d x y) | x < 0 || y < 0 = ColorRGBA 0 0 0 0
                        | ix >= length kern1d || iy >= length kern1d = ColorRGBA 0 0 0 0
                        | otherwise = ColorRGBA cc cc cc cc
                        where 
                        ix = round x
                        iy = round y
                        cc = (kern1d !! ix * kern1d !! iy) / totalsq
                        totalsq = sum kern1d ** 2
                        
-- | Convolution!
convolve :: Stencil -> ImageRGBA -> ImageRGBA
convolve kern2 img1 (Point2d px py) = reduceFn kern2 combinedList
         where 
         (xdim, ydim) = dim kern2
         (cx, cy) = (xdim `div` 2, ydim `div` 2)
         coordList = [ (kx, ky) | kx <- [0..xdim-1], ky <- [0..ydim - 1] ]
         translatedImg = img1 . translate (Point2d (-px + cx) (-py + cy))
         combinedList = [ pf (k2 (kx, ky)) (translatedImg (kx, ky)) | (kx, ky) <- coordList ]
         pf = ptwiseFn kern2
         k2 = kernel kern2

{-
boolBox :: Int -> Int -> Point2D -> Bool
boolBox xdim ydim (x, y) | x < 0 || y < 0 = False
                         | x >= xdim || y >= ydim = False
                         | otherwise = True
-}