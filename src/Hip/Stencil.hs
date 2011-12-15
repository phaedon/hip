{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-} 

module Hip.Stencil where

import Hip.PointSpace
import Hip.ColorSpace
import Hip.Image
import Hip.Transform
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed ((!))

gauss3x3, gauss5x5, gauss7x7, gauss9x9 :: ImageRGBA
gauss3x3 = outerProd $ VU.fromList [1, 2, 1]
gauss5x5 = outerProd $ VU.fromList [1, 4, 6, 4, 1]
gauss7x7 = outerProd $ VU.fromList [1, 6, 15, 20, 15, 6, 1]
gauss9x9 = outerProd $ VU.fromList [1, 8, 28, 56, 70, 56, 28, 8, 1]


-- | returns a 2d kernel function from a 1d kernel (by taking the 
--   outer product with itself
outerProd :: VU.Vector Double -> Point2d -> ColorRGBA
outerProd kern1d (Point2d x y) | x < 0 || y < 0 = colorEmpty
                        | ix >= VU.length kern1d || iy >= VU.length kern1d = colorEmpty
                        | otherwise = ColorRGBA cc cc cc cc
                        where 
                        ix = round x
                        iy = round y
                        cc = (kern1d ! ix * kern1d ! iy) / totalsq
                        totalsq = VU.sum kern1d ** 2


--convolve :: (ImageSYM a) => a -> a -> BBox2d -> Int -> Int -> Point2d -> t
convolve :: ImageSYM (Point2d -> t) => (Point2d -> t) -> (Point2d -> t) -> BBox2d -> Int -> Int -> Point2d -> t
convolve kern img bbox xdim ydim pt = reduce bbox cAdd (binary cMult kern (spatial shift img)) pt
         where

         -- get the kernel's center point
         kernCtr = Point2d (fromIntegral (xdim `div` 2)) (fromIntegral (ydim `div` 2))

         shift = translate $ pAdd (pNeg pt) kernCtr
