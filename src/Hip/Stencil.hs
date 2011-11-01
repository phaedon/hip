module Hip.Stencil where

import Hip.PointSpace
import Hip.ColorSpace
import Hip.Image
--import Data.List

-- | A Stencil is an image with fixed dimensions
--   TODO: This ought to take an operator that combines the values with 
--   a pixel from the image. that would define this thing more generally.
data Stencil = Stencil {
     kernel :: Kernel2D,
     dim :: (Int, Int) -- dimensions
     }

gauss3x3, gauss5x5, gauss7x7 :: Stencil
gauss3x3 = Stencil (outerProd [1, 2, 1]) (3, 3)
gauss5x5 = Stencil (outerProd [1, 4, 6, 4, 1]) (5, 5)
gauss7x7 = Stencil (outerProd [1, 6, 15, 20, 15, 6, 1]) (7, 7)

-- | returns a 2d kernel function from a 1d kernel (by taking the 
--   outer product with itself
outerProd :: [Float] -> Point2D -> Float
outerProd kern1d (x, y) | x < 0 || y < 0 = 0
                        | x >= length kern1d || y >= length kern1d = 0
                        | otherwise = (kern1d !! x * kern1d !! y) / totalsq
                        where 
                        totalsq = (sum kern1d) ** 2
                        

--stencil image xres yres 

convolve :: Stencil -> Image -> Image
convolve kern2 img1 (px, py) = floatToRGBA $ sumColors ptwise
         where 
         (xdim, ydim) = dim kern2
         (cx, cy) = (xdim `div` 2, ydim `div` 2)
         coordList = [ (kx, ky) | kx <- [0..xdim-1], ky <- [0..ydim - 1] ]
         ptwise = [ flScale (img2 (kx, ky)) (rgbaToFloat $ img1( px + kx - cx, py + ky - cy ))
                  | (kx, ky) <- coordList ]
         img2 = kernel kern2
         sumColors = foldr (flCAdd) (0,0,0,0)

