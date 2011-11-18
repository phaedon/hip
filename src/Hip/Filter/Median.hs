module Hip.Filter.Median (medianFilter) where

import Hip.Histogram
import Hip.Image
import Hip.Buffer
import Hip.ColorSpace

import Prelude hiding ((++))

import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed ((++))

import qualified Data.Map as Map
       
-- | Performs a median filter of radius krad on an image,
-- and returns a function that contains a buffer with the 
-- filtered image. 
-- Since we are using the optimized algorithm, this of course
-- requires evaluating the image at this stage in the pipeline.
medianFilter :: ImageRGBA -> BBox2d -> Int -> IO ImageRGBA
medianFilter img bbox krad = do 

             let ncols = round $ width bbox
             let nrows = round $ height bbox

             let ylim = nrows
             let xlim = ncols

             -- Loop that computes median for each pixel
             -- and inserts the color into the new color buffer.
             let loop cache v i j | j == ylim = return v
                          | i == xlim = loop cache v 0 (j + 1)
                          | otherwise = loop hCache (v ++ currColors) (i+1) j
                          where
                          (ColorRGBA r g b a) = median
                          currColors = VU.fromList [r, g, b, a]
                          (hCache, kHist) = kernelHist img cache (i, j) krad
                          median = kMedian kHist

                                                      
             mvec <- loop Map.empty VU.empty 0 0

             return $ crop bbox ( 
                      leaf $ 
                      dVectorToImageRGBA mvec (round $ width bbox) )

