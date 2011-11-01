import Hip.Generator
import Hip.ImageLoader
import Hip.Composite
--import Hip.Transform
import Ohiio.ImageOutput

main :: IO Bool
main = do
       tanyaFun <- mkImageFnFromFile "tanya.png"
       starFun <- mkImageFnFromFile "stars.png"
       
       let checker = genCheckerboard (10, 10) (0, 0, 0, 255) (255, 255, 255, 255)
       writeImage (pdPlus checker (pdXor  starFun tanyaFun)) (2000, 2400) "starry2.png"

       --writeImage (tanyaFun . (rotate (pi/4))) (2000, 2400) "rotated.png"

  --      writeImage tanyaFun (1000, 1000) "tanya_out.png"
       --writeImage (checker . rotate (pi/4)) (640, 480) "checker_rot.png"
