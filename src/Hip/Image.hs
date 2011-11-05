{-# LANGUAGE NoMonomorphismRestriction #-}

-- | This is the core of the imaging DSL.
module Hip.Image where

import Hip.ColorSpace
import Hip.PointSpace


----------------------------------------
-- Image TYPECLASSES
----------------------------------------


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


class ImageSYM repr where
      leaf :: (p -> a) -> repr (p -> a)
      unary :: (a -> b) -> repr (p -> a) -> repr (p -> b)
      binary :: (a -> b -> c) -> repr (p -> a) -> repr (p -> b) -> repr (p -> c)
      spatial :: (p -> p) -> repr (p -> c) -> repr (p -> c)

newtype FImage c = FImage { getFn :: c }

instance ImageSYM FImage where
      leaf fn = FImage fn
      unary op child = FImage $ op . (getFn child)
--      binary op tl tr = FImage $ op . (getFn tl) . (getFn tr)
      binary _ _ _  = undefined
      spatial op img = FImage $ getFn img . op


--    lit :: Int -> repr
  --  neg :: repr -> repr
    --add :: repr -> repr -> repr


-- | The most basic image typeclass.
-- The only thing we guarantee is that it can be converted
-- to an ImageRGBA. 
class Image i where
      initImage :: (Color c) => c -> i c

class ImgFunctor i where
      imap :: (c -> c) -> i c -> i c
      ilift2 :: (c -> c -> c) -> i c -> i c -> i c
--      ilift2 :: (a -> b -> c) -> i a -> i b -> i c
      

-- | An image that can be composited with another of its same type.
--  Because the Porter-Duff ops involve computations with alpha and
--  scaling factors, not all images can implement these computations
--  (for example, boolean images)
class CompositeImage i where
      iScale :: (CompositeColor c) => Double -> i c -> i c
      iOver :: (CompositeColor c) => i c -> i c -> i c
      iIn :: (CompositeColor c) => i c -> i c -> i c
      iOut :: (CompositeColor c) => i c -> i c -> i c
      iAtop :: (CompositeColor c) => i c -> i c -> i c
      iXor :: (CompositeColor c) => i c -> i c -> i c
      iPlus :: (CompositeColor c) => i c -> i c -> i c

      iDarken :: (CompositeColor c) => Double -> i c -> i c
      iDissolve :: (CompositeColor c) => Double -> i c -> i c
      iOpaque :: (CompositeColor c) => Double -> i c -> i c


instance Image ImageTree2d where
         initImage color = Leaf (\_ -> color)


-- | Defines pointwise operations on images!
instance ImgFunctor ImageTree2d where
         imap = Unary
         ilift2 = Binary

instance CompositeImage ImageTree2d where
         iScale s = imap $ cScale s

         iOver = ilift2 cOver
         iIn = ilift2 cIn
         iOut = ilift2 cOut
         iAtop = ilift2 cAtop
         iXor = ilift2 cXor
         iPlus = ilift2 cPlus

         iDarken s = imap $ cDarken s
         iDissolve s = imap $ cDissolve s
         iOpaque s = imap $ cOpaque s

type Kernel2d = (Int, Int) -> Double


eval :: ImageTree2d c -> Point2d -> c
eval (Leaf fn) p = fn p
eval (Unary op tree) p = op (eval tree p)
eval (Binary op t1 t2) p = op (eval t1 p) (eval t2 p)
eval (Spatial fn tree) p = (eval tree . fn) p
