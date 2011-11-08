import Hip.Image
import Hip.ColorSpace
import Hip.PointSpace
import Hip.Generator
import Hip.ImageLoader
import Hip.Transform
import Hip.ImageSaver
import Hip.Stencil

tanya :: (ImageSYM a) => IO a
tanya = mkImageFnFromFile "tanya.png"

main :: IO Bool
main = do

       --let shiftChecker = spatial (rotate (pi/10)) testChecker
       --let blurryCheck = convolve gauss9x9 shiftChecker (BBox2d pointOrigin 9 9)
       --saveImage blurryCheck (BBox2d pointOrigin 300 300) "blurryCheckerOut.png"
       
       --let grnfn = binary cOver (leaf testChecker) (leaf (\_ -> ColorRGBA 0 1 0 1))
       --saveImage grnfn (BBox2d pointOrigin 640 480) "grnout.png"

       -- grab the image from file
       --tanyaImg <- tanya

       -- shift up
       --let shiftTanya = spatial (translate (Point2d 0 (-300))) tanyaImg       

       -- grey, rotate and blur
       --let rotTanya =  spatial (rotate (pi/10)) (unary (cToGrey) shiftTanya)
       --let blurTanya = convolve gauss5x5 rotTanya (BBox2d pointOrigin 5 5)

       -- output a crop of the shifted image       
       --saveImage blurTanya (BBox2d (Point2d 1000 1000) 640 480)  "tanyaout.png"

       -- SLOW BLURRY CHECKERS
       --let blurChecker = convolve gauss3x3 testChecker (BBox2d pointOrigin 3 3)
       let blurChecker = convolve gauss7x7 testChecker (BBox2d pointOrigin 7 7) 7 7
       saveImage blurChecker (BBox2d pointOrigin 640 480) "blurCheckerOut.png"

       --starFun <- mkImageFnFromFile "stars.png"

       --chFun <- mkImageFnFromFile "normalchecker.png"       

       --let convGauss3 = convolve gauss3x3
       --let convGauss5 = convolve gauss5x5
       --let convGauss9 = convolve gauss9x9

       --let blurryT = convGauss7 tanyaFun 

       --let checker = genCheckerboard (10, 10) (0, 0, 0, 255) (255, 255, 255, 255)
       --let blurryC3 = convGauss3 checker
       --let blurryC5 = convGauss5 checker
       --let blurryC9 = convGauss9 checker

       --writeImage blurryC3 (640, 480) "blurrychecker3.png"
       --writeImage blurryC5 (640, 480) "blurrychecker5.png"
       --writeImage checker (320, 240) "normalchecker.png"
       --writeImage blurryC9 (320, 240) "blurrychecker9.png"
       --writeImage (convolve gauss9x9 chFun) (320, 240) "blurry.png"

       --writeImage (tanyaFun . translate (50, 50)) (640, 480) "ts_transl.png"
       --writeImage blurryT (2400, 2600) "blurrytanya.png"
       --writeImage (pdPlus checker (pdXor  starFun tanyaFun)) (2000, 2400) "starry2.png"
       --writeImage (tanyaFun . (rotate (pi/4))) (2000, 2400) "rotated.png"
       --writeImage tanyaFun (1000, 1000) "tanya_out.png"
       --writeImage (checker . rotate (pi/4)) (640, 480) "checker_rot.png"

       -- writeImage ((binary 0.2) tanyaFun) (2500, 2500) "tanya_out.png"
