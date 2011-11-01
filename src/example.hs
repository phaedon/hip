import Hip.Generator
--import Hip.ImageLoader
--import Hip.Composite
--import Hip.Transform
import Ohiio.ImageOutput
import Hip.Stencil

main :: IO Bool
main = do
       --tanyaFun <- mkImageFnFromFile "tanya.png"
       --starFun <- mkImageFnFromFile "stars.png"

       --let convGauss3 = convolve gauss3x3
       --let convGauss5 = convolve gauss5x5
       let convGauss7 = convolve gauss7x7

       --let blurryT = convGauss7 tanyaFun 

       let checker = genCheckerboard (10, 10) (0, 0, 0, 255) (255, 255, 255, 255)
       --let blurryC3 = convGauss3 checker
       --let blurryC5 = convGauss5 checker
       let blurryC7 = convGauss7 checker
       --_ <- writeImage blurryC3 (640, 480) "blurrychecker3.png"
       --_ <- writeImage blurryC5 (640, 480) "blurrychecker5.png"
       writeImage blurryC7 (640, 480) "blurrychecker7.png"
       
       --writeImage blurryT (2400, 2600) "blurrytanya.png"
       --
       --       writeImage (pdPlus checker (pdXor  starFun tanyaFun)) (2000, 2400) "starry2.png"

       --writeImage (tanyaFun . (rotate (pi/4))) (2000, 2400) "rotated.png"

  --      writeImage tanyaFun (1000, 1000) "tanya_out.png"
       --writeImage (checker . rotate (pi/4)) (640, 480) "checker_rot.png"
