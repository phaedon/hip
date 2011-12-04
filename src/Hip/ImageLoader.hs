module Hip.ImageLoader where

import Hip.Image
import Hip.ColorSpace
import Hip.PointSpace
import Ohiio.ImageInput
import Ohiio.ImageSpec
import OhiioBindings

import System.IO.Unsafe
import Foreign.Storable


-- TODO: This function leaks memory, because it wraps the image's
-- mem buffer in a closure. Must fix!
-- TODO: 

-- | Uses the Ohiio library to load an image from disk, and turns it
-- into a Hip-compatible cropped image expression.
mkImageFnFromFile :: (ImageSYM a) => String -> IO a
mkImageFnFromFile filename = do

                  -- Read the image and grab the relevant pointers
                  (mem, iSpecPtr) <- readImage filename

                  -- image size info
                  imgDims <- toHImageSpec iSpecPtr

                  let w = imgWidth imgDims
                  let h = imgHeight imgDims
                  let ch = imgNChannels imgDims

                  let bbox = BBox2d pointOrigin (fromIntegral w) (fromIntegral h)

                  -- Partial function application allows us to return a closure.
                  -- SPECIAL TREAT: now, the crop box is part of the expression,
                  -- so we should be able to interpret it later and move it around.
                  return $ crop bbox (leaf ( image mem ch w ))
                  
                  where
                  image :: BytePtr -> Int -> Int -> Point2d -> ColorRGBA
                  image mem chan w (Point2d x y)
                        =  rgba8ToRGBA $ toColor (unsafePerformIO $ peekByteOff mem offset)
                        where
                        offset = chan * (round y * w + round x)

