module Hip.Color.Types where

import Hip.Color.Space
import Hip.Color.Composite

import Data.Word



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

instance Color ColorRGBA8 where
         toColorRGBA (ColorRGBA8 r8 g8 b8 a8) = ColorRGBA rf gf bf af
                   where 
                   toDouble :: Word8 -> Double
                   toDouble w = (fromIntegral w) / 255
                   rf = toDouble r8
                   gf = toDouble g8
                   bf = toDouble b8
                   af = toDouble a8

data ColorBool = ColorBool !Bool

instance Color ColorBool where
         toColorRGBA (ColorBool b) | b = ColorRGBA 1 1 1 1
                                   | otherwise = ColorRGBA 0 0 0 0
