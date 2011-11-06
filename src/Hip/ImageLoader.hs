module Hip.ImageLoader where

import Hip.Image
import Hip.ColorSpace
import Hip.PointSpace
import Ohiio.ImageInput
import OhiioBindings

import System.IO.Unsafe
import Foreign.Storable


-- TODO: This function leaks memory, because it wraps the image's
-- mem buffer in a closure. Must fix!
mkImageFnFromFile :: (ImageSYM a) => String -> IO a
mkImageFnFromFile filename = do

                  -- Read the image and grab the relevant pointers
                  (mem, iSpecPtr) <- readImage filename

                  -- image size info
                  width <- c_ImageSpec_width iSpecPtr 
                  height <- c_ImageSpec_height iSpecPtr 
                  nchannels <- c_ImageSpec_nchannels iSpecPtr                        

                  let dims = Point2d (fromIntegral width) (fromIntegral height)
                  let w = fromIntegral width
                  let ch = fromIntegral nchannels                  

                  -- Partial function application allows us to return a closure
                  return $ crop (Point2d 0 0) dims (leaf ( image mem ch w ))
                  
                  where
                  image :: BytePtr -> Int -> Int -> Point2d -> ColorRGBA
                  image mem chan w (Point2d x y) 
                        =  rgba8ToRGBA $ toColor (unsafePerformIO $ peekByteOff mem offset)
                        where
                        offset = chan * (round y * w + round x)

