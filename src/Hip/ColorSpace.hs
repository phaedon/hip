module Hip.ColorSpace where

import Data.Word


data PrimaryColor = Red | Green | Blue | Alpha
     deriving (Eq, Show)


-----------------------------
-- COLOR TYPECLASSES
-----------------------------

-- | The Color class simply requires any color type to be convertible to Double.
--   Why? because all computations will be done in this space, without dealing 
--   with the unpleasantries of the Word8 restriction (such as the pain of 
--   having to round everything and losing precision.)
-- 
--   Note that there is no method for converting FROM Word8. 
--   Although the need for this is obvious when loading a file from disk,
--   we cannot convert to EVERY color type (i.e. bool values in morphological
--   images) without an additional argument (i.e. a thresholding constant).
--   We'll leave that to a specific extension.
class Color repr where 
      toColorRGBA :: repr -> ColorRGBA


{--
Sources:
========
- Compositing Digital Images. Porter, T., & Duff, T. (1984). 
  Computer Graphics. Computer, 18(3), 253-259.
- Functional Image Synthesis. Elliott, C. 

--}

class (Color repr) => CompositeColor repr where
      cScale :: Double -> repr -> repr

      cOver :: repr -> repr -> repr
      cIn :: repr -> repr -> repr
      cOut :: repr -> repr -> repr
      cAtop :: repr -> repr -> repr
      cXor :: repr -> repr -> repr
      cPlus :: repr -> repr -> repr

      cDarken :: Double -> repr -> repr
      cDissolve :: Double -> repr -> repr
      cOpaque :: Double -> repr -> repr



-----------------------------
-- COLOR TYPES
-----------------------------



-- | This is the most important type in our system. All computations
--   will be done in this space, because:
--   (a) Doubles are fast (in Haskell)
--   (b) No loss of precision
--   (c) Easily supports all compositing ops
--   (d) We can defer truncating and rounding to the end
data ColorRGBA = ColorRGBA {
     red :: !Double,
     green :: !Double,
     blue :: !Double,
     alpha :: !Double
} deriving (Show, Eq)
     
instance Color ColorRGBA where
         toColorRGBA = id

-- | Implementation of Porter-Duff compositing algebra
instance CompositeColor ColorRGBA where
         
         cScale s (ColorRGBA r g b a) = ColorRGBA (s*r) (s*g) (s*b) (s*a)

         cOver c1 c2 = cPlus c1 (cScale (1 - alpha c1) c2)

         cIn c1 c2 = cScale (alpha c2) c1

         cOut c1 c2 = cScale (1 - alpha c2) c1

         cAtop c1 c2 = cPlus (cScale (alpha c2) c1) (cScale (1 - alpha c1) c2)

         cXor c1 c2 = cPlus (cScale (1 - alpha c2) c1) (cScale (1 - alpha c1) c2)

         cPlus (ColorRGBA r1 g1 b1 a1) (ColorRGBA r2 g2 b2 a2)
               = ColorRGBA (r1 + r2) (g1 + g2) (b1 + b2) (a1 + a2)

         cDarken phi (ColorRGBA r g b a) = ColorRGBA (phi*r) (phi*g) (phi*b) a
         
         cDissolve = cScale

         cOpaque omega (ColorRGBA r g b a) = ColorRGBA r g b (a * omega)



data ColorRGBA8 = ColorRGBA8 !Word8 !Word8 !Word8 !Word8
     deriving (Show, Eq)

instance Color ColorRGBA8 where
         toColorRGBA (ColorRGBA8 r8 g8 b8 a8) = ColorRGBA rf gf bf af
                   where 
                   toDouble :: Word8 -> Double
                   toDouble w = fromIntegral w / 255
                   rf = toDouble r8
                   gf = toDouble g8
                   bf = toDouble b8
                   af = toDouble a8


data ColorBool = ColorBool !Bool

instance Color ColorBool where
         toColorRGBA (ColorBool b) | b = ColorRGBA 1 1 1 1
                                   | otherwise = ColorRGBA 0 0 0 0
