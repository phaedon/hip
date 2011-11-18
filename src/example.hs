import Hip.Image
import Hip.ColorSpace
import Hip.PointSpace
import Hip.Generator
import Hip.ImageLoader
import Hip.Transform
import Hip.ImageSaver
import Hip.Stencil
import Hip.Histogram
import Hip.Filter.Median

import qualified Data.Map as Map

tanya :: (ImageSYM a) => IO a
tanya = mkImageFnFromFile "tanya.png"

hey :: IO CHist
hey = do
    tanyaImg <- tanya
    return $ createColumnHist tanyaImg 12 100 1000

main :: IO Bool
main = do
       
       -- grab the image from file
       --tanyaImg <- tanya

--       checkImg <- mkImageFnFromFile "medchecker.png"

       --let medianTanya = toMed tanyaImg
--       let medChecker = toMed checkImg

       -- 100x100 with rad 3 filter: 1min 19sec
       --    1min 10sec by getting rid of redundant cons commands?
       
       putStrLn "Beginning median filter..."

       medChecker <- medianFilter testChecker (BBox2d pointOrigin 100 100) 3

       putStrLn "Done!"
       putStrLn "Saving image..."

       -- shift up
       --let shiftTanya = spatial (translate (Point2d 0 (-200))) tanyaImg       

       -- grey, rotate and scale
       --let rotTanya =  spatial (rotate (pi/10)) (unary (cToGrey) shiftTanya)

       -- output a crop of the shifted image       
       --saveImage medianTanya (BBox2d (Point2d 800 800) 200 200)  "tanyaout.png"
       saveImage medChecker (BBox2d pointOrigin 100 100)  "medcheckerout.png"

       -- SLOW BLURRY CHECKERS
       --let shiftChecker = spatial (rotate (pi/10)) testChecker
       --let blurChecker = convolve gauss3x3 testChecker (BBox2d pointOrigin 3 3) 3 3
       --let blurChecker = convolve gauss7x7 testChecker (BBox2d pointOrigin 7 7) 7 7
       --saveImage blurChecker (BBox2d pointOrigin 640 480) "blurCheckerOut.png"
{-           where
           toMed :: (Point2d -> ColorRGBA) -> Point2d -> ColorRGBA
           toMed img pt = kMedian khist
                 where
                 (x, y) = roundPoint pt
                 (m, khist) = kernelHist img Map.empty (x, y) 2 -- bigass radius
       
  -}     
       