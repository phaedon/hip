module Hip.ImageLoader where

import Hip.Image
import Hip.ColorSpace
import Ohiio.ImageInput
import OhiioBindings

import System.IO.Unsafe
import Foreign.Storable


-- TODO: This function leaks memory, because it wraps the image's
-- mem buffer in a closure. Must fix!
mkImageFnFromFile :: String -> IO ImageRGBA
mkImageFnFromFile filename = do

                  -- Read the image and grab the relevant pointers
                  (mem, iSpecPtr) <- readImage filename

                  -- image size info
                  width <- c_ImageSpec_width iSpecPtr 
                  height <- c_ImageSpec_height iSpecPtr 
                  nchannels <- c_ImageSpec_nchannels iSpecPtr                        

                  let dims = (fromIntegral width, fromIntegral height, fromIntegral nchannels)

                  -- Partial function application allows us to return a closure
                  return $ image mem dims
                  
                  where
                  image :: BytePtr -> (Int, Int, Int) -> ImageRGBA
                  image mem (w, h, chan) (x, y) 
                        -- bounds checking
                        | x >= w = (0,0,0,0)
                        | y >= h = (0,0,0,0)
                        | x < 0 = (0,0,0,0)
                        | y < 0 = (0,0,0,0)

                        -- We're safe. Retrieve!
                        | otherwise = toColor (unsafePerformIO $ peekByteOff mem offset)

                        where
                        offset = chan * (y * w + x)

