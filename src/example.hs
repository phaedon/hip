{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-} 


import Hip.Image
import Hip.ColorSpace
import Hip.PointSpace
import Hip.Generator
import Hip.ImageLoader
--import Hip.Transform
import Hip.ImageSaver
import Hip.Stencil
--import Hip.Histogram
import Hip.Filter.Median

--import qualified Data.Map as Map


tanya :: IO ImageRGBA
tanya = mkImageFnFromFile "tanya.png"

main :: IO Bool
main = do
       
       let c = leaf testChecker
       let cc = crop (BBox2d pointOrigin 100 100) c
       let ccc = unary (cDarken 0.8) cc

       
       
       let cccc = crop (BBox2d (Point2d 5 5) 25 50) ccc       

       let ccccc = convolve gauss7x7 cccc (BBox2d pointOrigin 7 7) 7 7 

       let pushedCheck = push_crop cccc

       let ev = eval pushedCheck
       let bb = boxify pushedCheck

       saveImage ev bb "ccc.png"

       {-
       lambros <- mkImageFnFromFile "lambros.png"
       medLambros <- medianFilter lambros (BBox2d pointOrigin 640 360) 1
       saveImage medLambros (BBox2d pointOrigin 640 360)  "medLambros.png"
       -}