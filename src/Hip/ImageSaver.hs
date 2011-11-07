module Hip.ImageSaver where

import Hip.Image
import Hip.ColorSpace
import Hip.PointSpace

import Ohiio.ImageOutput
import OhiioBindings

import Data.Word
import Foreign.Ptr
--import Foreign.C.Types
--import Foreign.C.String -- for newCString
import Foreign.Storable
--import Foreign.Marshal.Alloc -- for malloc & free

import Data.ByteString
import qualified Data.ByteString.Internal as BS


-- |Modifies a 32-bit word in a memory buffer, 
-- based on a 2D interpretation of the space.
-- Color comes from provided image function.
poker :: BytePtr -> ColorRGBA8 -> Int -> (Int, Int) -> IO ()
poker wptr (ColorRGBA8 r g b a) xres (x, y) = do
      pokeByteOff wptr (4 * (y * xres + x)) r
      pokeByteOff wptr (4 * (y * xres + x) + 1) g
      pokeByteOff wptr (4 * (y * xres + x) + 2) b
      pokeByteOff wptr (4 * (y * xres + x) + 3) a


populateBuffer :: ImageRGBA -> BBox2d -> Ptr Word8 -> IO ()
populateBuffer img (BBox2d (Point2d cx cy) w h) wptr = do

               let icx = round cx
               let icy = round cy
               let intWidth = round w
               let xlim = round w
               let ylim = round h

               -- define double loop over monadic actions
               let loop i j | j == ylim = return ()
                            | i == xlim = loop 0 (j+1)
                            | otherwise = 
                    poker wptr currColor intWidth (i, j) >> loop (i+1) j
                    where 
                    currColor = evalRGBA8 img (createPoint (i + icx, j + icy))

               loop 0 0


imageToByteString :: ImageRGBA -> BBox2d -> IO ByteString
imageToByteString img bbox = do

                  -- Partial function app to create closure
                  let filler = populateBuffer img bbox

                  -- and, create the memory buffer, properly initialized!
                  BS.create (bufferSize bbox) filler
                  
saveImage :: ImageRGBA -> BBox2d -> String -> IO Bool
saveImage img bbox name = do
          
          let w = round $ width bbox
          let h = round $ height bbox
          buf <- imageToByteString img bbox
          writeImage buf (w, h) name