module Hip.Filter.Median (medianFilter) where

import Hip.Histogram
import Hip.Image
import Hip.Buffer
import Hip.ColorSpace

import Prelude hiding ((++))

import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed ((++))

import qualified Data.Map as Map

import Text.Printf

import Data.Word
import Foreign.Ptr

import Data.ByteString
import qualified Data.ByteString.Internal as BS

import Hip.ImageSaver
       
-- | Performs a median filter of radius krad on an image,
-- and returns a function that contains a buffer with the 
-- filtered image. 
-- Since we are using the optimized algorithm, this of course
-- requires evaluating the image at this stage in the pipeline.
medianFilter' :: ImageRGBA -> BBox2d -> Int -> Ptr Word8 -> IO ()
medianFilter' img bbox krad wptr = do 

             let ncols = round $ width bbox
             let nrows = round $ height bbox

             let ylim = nrows
             let xlim = ncols

             -- Loop that computes median for each pixel
             -- and inserts the color into the new color buffer.
             let loop cache i j | j == ylim = return ()
                          | i == xlim = loop cache 0 (j + 1)
                          | otherwise = do
                                      poker wptr median xlim (i, j)
                                      loop hCache (i+1) j
                          where
                          (hCache, kHist) = kernelHist img cache (i, j) krad
                          median = rgbaToRGBA8 $ kMedian kHist

             loop Map.empty 0 0

medianFilter :: ImageRGBA -> BBox2d -> Int -> IO ImageRGBA
medianFilter img bbox krad = do
                let filler = medianFilter' img bbox krad
                bs <- BS.create (bufferSize bbox) filler
                
                return $ crop bbox (
                       leaf $ 
                       bytestringToImageRGBA bs (round $ width bbox) )