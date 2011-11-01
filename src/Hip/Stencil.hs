module Hip.Stencil where

---import Hip.PointSpace
import Hip.ColorSpace
import Hip.Image

-- | A Stencil is an image with fixed dimensions
data Stencil = Stencil {
     kernel :: Kernel2D,
     dim :: (Int, Int) -- dimensions
     }

gauss3x3 :: Stencil
gauss3x3 = Stencil k (xdim, ydim)
         where

         (xdim, ydim) = (3, 3)
         -- super awkward to type! TODO better way of specifying kernel
         k (x, y) | x >= xdim || y >= ydim || x < 0 || y < 0 = 0
                  | (x, y) == (0,0) || (x, y) == (0, 2) 
                   || (x, y) == (2,0) || (x, y) == (2, 2) = 1/16
                  | (x, y) == (1, 1) = 4/16
                  | otherwise = 2/16

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

