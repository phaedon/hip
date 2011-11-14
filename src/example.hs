import Hip.Image
import Hip.ColorSpace
import Hip.PointSpace
import Hip.Generator
import Hip.ImageLoader
import Hip.Transform
import Hip.ImageSaver
import Hip.Stencil
import Hip.Histogram

tanya :: (ImageSYM a) => IO a
tanya = mkImageFnFromFile "tanya.png"

hey :: IO CHist
hey = do
    tanyaImg <- tanya
    return $ createColumnHist tanyaImg 12 100 1000

main :: IO Bool
main = do
       
       -- grab the image from file
       tanyaImg <- tanya

       -- shift up
       let shiftTanya = spatial (translate (Point2d 0 (-200))) tanyaImg       

       -- grey, rotate and scale
       let rotTanya =  spatial (rotate (pi/10)) (unary (cToGrey) shiftTanya)

       -- output a crop of the shifted image       
       saveImage rotTanya (BBox2d (Point2d 800 800) 1200 1000)  "tanyaout.png"

       -- SLOW BLURRY CHECKERS
       --let shiftChecker = spatial (rotate (pi/10)) testChecker
       --let blurChecker = convolve gauss3x3 testChecker (BBox2d pointOrigin 3 3) 3 3
       --let blurChecker = convolve gauss7x7 testChecker (BBox2d pointOrigin 7 7) 7 7
       --saveImage blurChecker (BBox2d pointOrigin 640 480) "blurCheckerOut.png"
       
       
       