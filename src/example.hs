import Hip.Image
import Hip.ColorSpace
import Hip.PointSpace
import Hip.Generator
import Hip.ImageLoader
--import Hip.Transform
import Hip.ImageSaver
--import Hip.Stencil
--import Hip.Histogram
import Hip.Filter.Median

import qualified Data.Map as Map

--import Criterion.Main

tanya :: (ImageSYM a) => IO a
tanya = mkImageFnFromFile "tanya.png"

--main = defaultMain [bench "medfil" $ whnfIO mainOld]

main :: IO Bool
main = do
       
       -- grab the image from file
       --tanyaImg <- tanya

--       checkImg <- mkImageFnFromFile "medchecker.png"

       --let medianTanya = toMed tanyaImg
--       let medChecker = toMed checkImg

----------------
-- TIMINGS
----------------
       -- 100x100 with rad 3 filter: 0.9 sec
       -- 100x100 with rad 7 filter: 4.4 sec
       -- 200x200 with rad 3 filter: 3.2 sec
       -- 350x350 with rad 10 filter: 115 sec
       -- 640x480 with rad 3 filter: 17 sec
       noisy <- mkImageFnFromFile "tanya.png"
       medNoisy <- medianFilter noisy (BBox2d pointOrigin 640 480) 3

       --medChecker <- medianFilter testChecker (BBox2d pointOrigin 100 100) 3

       -- shift up
       --let shiftTanya = spatial (translate (Point2d 0 (-200))) tanyaImg       

       -- grey, rotate and scale
       --let rotTanya =  spatial (rotate (pi/10)) (unary (cToGrey) shiftTanya)

       -- output a crop of the shifted image       
       --saveImage medianTanya (BBox2d (Point2d 800 800) 200 200)  "tanyaout.png"
       saveImage medNoisy (BBox2d pointOrigin 640 480)  "medTanya.png"
       --saveImage medChecker (BBox2d pointOrigin 100 100)  "medChecker.png"

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
       