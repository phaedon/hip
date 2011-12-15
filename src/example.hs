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

tanya :: IO ImageRGBA
tanya = mkImageFnFromFile "tanya.png"

main :: IO Bool
main = do
       
       let c = leaf testChecker
       let cc = crop (BBox2d pointOrigin 100 100) c
       let ccc = unary (cDarken 0.8) cc
       
       let cccc = crop (BBox2d (Point2d 5 5) 25 50) ccc       

       let pushedCheck = push_crop cccc

       saveImage pushedCheck (BBox2d pointOrigin 640 360) "ccc.png"

       {-
       lambros <- mkImageFnFromFile "lambros.png"
       medLambros <- medianFilter lambros (BBox2d pointOrigin 640 360) 1
       saveImage medLambros (BBox2d pointOrigin 640 360)  "medLambros.png"
       -}