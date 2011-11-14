module Hip.Filter.Median where


import Hip.Histogram

import Hip.ColorSpace
import Hip.PointSpace
import Hip.Image

import qualified Data.Vector.Unboxed as VU

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

             -- Initialize a set of column histograms over the image
             hist <- return $ initImageHist img ncols krad

             
             let loop i j | j == ylim = return ()
                          | i == xlim = loop 0 (j + 1)
                          | otherwise = undefined
                          
             loop 0 0
             
             return $ getColor VU.empty

             where
             getColor :: VU.Vector ColorRGBA -> ImageRGBA
             getColor colorvec = img
             
             