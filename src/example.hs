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
       
       lambros <- mkImageFnFromFile "lambros.png"
       medLambros <- medianFilter lambros (BBox2d pointOrigin 640 360) 1
       saveImage medLambros (BBox2d pointOrigin 640 360)  "medLambros.png"
