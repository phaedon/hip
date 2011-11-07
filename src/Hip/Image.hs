{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-} 

-- | This module defines the core of the imaging DSL.
-- Much of the inspiration comes from Pan and work on tagless interpreters.
-- For more information, please see:
-- http://conal.net/Pan/
-- http://okmij.org/ftp/tagless-final/course/course.html
module Hip.Image where

import Hip.Lift
import Hip.ColorSpace
import Hip.PointSpace


----------------------------------------
-- Image TYPECLASSES
----------------------------------------

type ImageRGBA = Point2d -> ColorRGBA
type ColorXform = ColorRGBA -> ColorRGBA
type PointXform = Point2d -> Point2d
type ColorMerge = ColorRGBA -> ColorRGBA -> ColorRGBA

class ImageSYM repr where
      leaf :: ImageRGBA -> repr
      unary :: ColorXform -> repr -> repr
      binary :: (ColorRGBA -> ColorRGBA -> ColorRGBA) -> repr -> repr -> repr
      spatial :: PointXform -> repr -> repr
      crop :: BBox2d -> repr -> repr
      reduce :: BBox2d -> ColorMerge -> repr -> repr

-- | For now, this is redundant. But I'm imagining adding bounding boxes
-- and other interesting stuff...
newtype FImage = FImage { getFn :: Point2d -> ColorRGBA }

instance ImageSYM FImage where
      leaf = FImage
      unary op child = FImage $ lift1 op (getFn child)
      binary op tl tr = FImage $ lift2 op (getFn tl) (getFn tr)
      spatial op img = FImage $ getFn img . op
      crop = undefined 
      reduce = undefined

-- | OMG conversion to data type. How cool, how bizarre
instance ImageSYM (ImageTree2d ColorRGBA) where
         leaf = Leaf
         unary = Unary
         binary = Binary
         spatial = Spatial
         crop = undefined
         reduce = undefined

-- | An image expression as a single function. Whoa.......
instance ImageSYM (Point2d -> ColorRGBA) where
         leaf = id
         unary = lift1
         binary = lift2
         spatial op img = img . op
         crop bbox img pt | isInside bbox pt = img pt
                          | otherwise = colorEmpty
         -- TODO: add an argument for init value (colorEmpty won't always work)
         reduce bbox op img _ = foldr op colorEmpty colors
                where
                colors = [img p | p <- (bboxToCoordList bbox)]
         

instance ImageSYM [Char] where
         leaf _ = "Leaf"
         unary _ child = "Unary" ++ "[" ++ child ++ "]"
         binary _ tl tr = "Binary: " ++ "[" ++ tl ++ ", " ++ tr ++ "]"
         spatial _ img = "Spatial: " ++ "[" ++ img ++ "]"
         crop bbox img = "Crop: " 
                       ++ "Corner: " ++ show (corner bbox) 
                       ++ "Dims: " ++ show (width bbox, height bbox) 
                       ++ "[" ++ img ++ "]"
         reduce = undefined
                       

wtf :: ImageTree2d ColorRGBA -> ImageTree2d ColorRGBA
wtf = id

eval :: (Point2d -> ColorRGBA) -> Point2d -> ColorRGBA
eval fn = fn

evalRGBA8 :: (Point2d -> ColorRGBA) -> Point2d -> ColorRGBA8
evalRGBA8 fn = rgbaToRGBA8 . fn

wrap :: FImage -> FImage
wrap = id

showStr :: String -> String
showStr = id



-- | I'm keeping this around for now, in case I need to do anything with it.
data ImageTree2d c
     = Leaf { imageFn :: Point2d -> c }
     | Unary { unaryOp :: c -> c,
               treePtr :: ImageTree2d c
               }
     | Binary { binaryOp :: c -> c -> c,
                leftPtr :: ImageTree2d c,
                rightPtr :: ImageTree2d c
                }         
     | Spatial { xformFn :: Point2d -> Point2d,
                 streePtr :: ImageTree2d c
                 }       

data BBox2d = BBox2d { corner :: Point2d,
                       width :: Double,
                       height :: Double } deriving (Show)


bboxIntDims :: BBox2d -> (Int, Int)
bboxIntDims (BBox2d _ dw dh) = (round dw, round dh)

bufferSize :: BBox2d -> Int
bufferSize (BBox2d _ dw dh)
           = (round dw * round dh) * 4

isInside :: BBox2d -> Point2d -> Bool
isInside (BBox2d (Point2d cx cy) w h) (Point2d x y) 
         | x  < cx || x > cx + w || y < cy || y > cy + h = False
         | otherwise = True

bboxToCoordList :: BBox2d -> [Point2d]
bboxToCoordList (BBox2d (Point2d cx cy) w h) = coordList
                where
                icx = (round cx)::Int
                icy = (round cy)::Int
                iw = round w
                ih = round h
                coordList = [ Point2d (fromIntegral kx) (fromIntegral ky) | kx <- [icx..(icx + iw)], ky <- [icy..(icy + ih)] ]

                
                