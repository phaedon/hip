{-# LANGUAGE TypeSynonymInstances #-}

module Hip.Image where

import Hip.ColorSpace
import Hip.PointSpace
import Hip.Lift


class Image i where
      toImageRGBA :: i -> ImageRGBA
--      initImage :: (Color c) => c -> i

class CompositeImage c where
      iScale :: Double -> c -> c
      iOver :: c -> c -> c
      iIn :: c -> c -> c
      iOut :: c -> c -> c
      iAtop :: c -> c -> c
      iXor :: c -> c -> c
      iPlus :: c -> c -> c

      iDarken :: Double -> c -> c
      iDissolve :: Double -> c -> c
      iOpaque :: Double -> c -> c

type ImageRGBA = Point2d -> ColorRGBA

instance Image ImageRGBA where
         toImageRGBA = id
--         initImage c = lift0 c

instance CompositeImage ImageRGBA where
         iScale f = lift1 $ cScale f

         iOver = lift2 cOver
         iIn = lift2 cIn
         iOut = lift2 cOut
         iAtop = lift2 cAtop
         iXor = lift2 cXor
         iPlus = lift2 cPlus

         iDarken s = lift1 $ cDarken s
         iDissolve s = lift1 $ cDissolve s
         iOpaque s = lift1 $ cOpaque s



type Kernel2d = (Int, Int) -> Double

type ImageOUT = Point2dInt -> ColorRGBA8

toImageOUT :: ImageRGBA -> ImageOUT
toImageOUT = undefined 