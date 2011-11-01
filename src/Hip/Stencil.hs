module Hip.Stencil where

import Hip.PointSpace
import Hip.ColorSpace
import Hip.Image
import Hip.Transform


-- | A Stencil is an image with fixed dimensions
data Stencil = Stencil {
     kernel :: Kernel2D,
     dim :: (Int, Int), -- dimensions
     ptwiseFn :: Float -> RGBAColor -> FloatColor, -- this will become more generic
     reduceFn :: [FloatColor] -> RGBAColor  -- and so will this
     }


-- | Combines two points: the first is from the kernel,
--  the second from the image, and outputs a float color in [0,1]
--  i.e. this is pointwise multiplication
ptwiseConv :: Float -> RGBAColor -> FloatColor
ptwiseConv f c = flScale f (rgbaToFloat c)

reduceConv :: [FloatColor] -> RGBAColor
reduceConv cl = floatToRGBA (foldr flCAdd (0,0,0,0) cl)

gauss3x3, gauss5x5, gauss7x7, gauss9x9 :: Stencil
gauss3x3 = Stencil (outerProd [1, 2, 1]) (3, 3) ptwiseConv reduceConv
gauss5x5 = Stencil (outerProd [1, 4, 6, 4, 1]) (5, 5) ptwiseConv reduceConv
gauss7x7 = Stencil (outerProd [1, 6, 15, 20, 15, 6, 1]) (7, 7) ptwiseConv reduceConv
gauss9x9 = Stencil (outerProd [1, 8, 28, 56, 70, 56, 28, 8, 1]) (9, 9) ptwiseConv reduceConv


-- | returns a 2d kernel function from a 1d kernel (by taking the 
--   outer product with itself
outerProd :: [Float] -> Point2D -> Float
outerProd kern1d (x, y) | x < 0 || y < 0 = 0
                        | x >= length kern1d || y >= length kern1d = 0
                        | otherwise = (kern1d !! x * kern1d !! y) / totalsq
                        where 
                        totalsq = sum kern1d ** 2
                        
-- | Convolution!
convolve :: Stencil -> Image -> Image
convolve kern2 img1 (px, py) = reduceFn kern2 combinedList
         where 
         (xdim, ydim) = dim kern2
         (cx, cy) = (xdim `div` 2, ydim `div` 2)
         coordList = [ (kx, ky) | kx <- [0..xdim-1], ky <- [0..ydim - 1] ]
         translatedImg = img1 . translate (-px + cx, -py + cy)
         combinedList = [ pf (k2 (kx, ky)) (translatedImg (kx, ky)) | (kx, ky) <- coordList ]
         pf = ptwiseFn kern2
         k2 = kernel kern2

boolBox :: Int -> Int -> Point2D -> Bool
boolBox xdim ydim (x, y) | x < 0 || y < 0 = False
                         | x >= xdim || y >= ydim = False
                         | otherwise = True