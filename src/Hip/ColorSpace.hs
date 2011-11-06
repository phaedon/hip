-- | A mini-DSL for colors.
-- A "color" is extremely generic. In our system, it is anything that can be
-- the range of an Image.
-- Examples:
--  - Bool: for morphological image processing, or for masks
--  - RGBA8: traditional 32-bit representation
--  - Float: for greyscale
--  - Int: for accumulating histograms (where a histogram is an image with 
--    a 1-dimensional domain.
module Hip.ColorSpace where

import Data.Word 
import Data.Bits 


-----------------------------
-- COLOR TYPECLASSES
-----------------------------

{- | The Color class simply requires the ability to combine two colors,
using addition (OR). We don't even require multiplication, which
makes no sense for histograms, for example.
 
Although this typeclass seems sort of useless, it will allow us to write
very general definitions of images
-}
class Color color where 
      cAdd :: color -> color -> color


{-- |
Sources:
========
- Compositing Digital Images. Porter, T., & Duff, T. (1984). 
  Computer Graphics. Computer, 18(3), 253-259.
- Functional Image Synthesis. Elliott, C. 

--}
class (Color ccolor) => CompositeColor ccolor where
      cScale :: Double -> ccolor -> ccolor

      cOver :: ccolor -> ccolor -> ccolor
      cIn :: ccolor -> ccolor -> ccolor
      cOut :: ccolor -> ccolor -> ccolor
      cAtop :: ccolor -> ccolor -> ccolor
      cXor :: ccolor -> ccolor -> ccolor
      cPlus :: ccolor -> ccolor -> ccolor

      cDarken :: Double -> ccolor -> ccolor
      cDissolve :: Double -> ccolor -> ccolor
      cOpaque :: Double -> ccolor -> ccolor



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
         cAdd = cPlus

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
         cAdd c1 c2 = rgbaToRGBA8 combRGBA
              where
              combRGBA = cAdd (rgba8ToRGBA c1) (rgba8ToRGBA c2)
              
         
-- | Convert a Word8 4-tuple to floats in [0..1]
rgba8ToRGBA :: ColorRGBA8 -> ColorRGBA
rgba8ToRGBA (ColorRGBA8 r8 g8 b8 a8) = ColorRGBA rf gf bf af
             where 
             toDouble :: Word8 -> Double
             toDouble w = fromIntegral w / 255
             rf = toDouble r8
             gf = toDouble g8
             bf = toDouble b8
             af = toDouble a8

-- | Truncate a floating RGBA representation to [0..1]
truncRGBA :: ColorRGBA -> ColorRGBA
truncRGBA (ColorRGBA r g b a) = ColorRGBA rr rg rb ra
          where
           rr = if r > 1 then 1 else r
           rg = if g > 1 then 1 else g
           rb = if b > 1 then 1 else b
           ra = if a > 1 then 1 else a


-- | Convert a floating RGBA 4-tuple to Word8
rgbaToRGBA8 :: ColorRGBA -> ColorRGBA8
rgbaToRGBA8 c = ColorRGBA8 r8 g8 b8 a8
            where
            -- first, truncate (to ensure saturating semantics)
            (ColorRGBA r g b a) = truncRGBA c

            -- scale from [0..1] to [0..255]:
            r8 = round $ r * 255
            g8 = round $ g * 255
            b8 = round $ b * 255
            a8 = round $ a * 255


data ColorBool = ColorBool !Bool

instance Color ColorBool where
         cAdd (ColorBool a) (ColorBool b) = ColorBool $ a || b


-- | Given a color field, computes proper bitmask
colorMask :: PrimaryColor -> Word32
colorMask pc = shiftL 0x000000ff (colorShift pc)

-- | Returns bitshift given a color field
colorShift :: PrimaryColor -> Int
colorShift pc | pc == Red = 0
              | pc == Green = 8
              | pc == Blue = 16
              | pc == Alpha = 24
              | otherwise = error "No such color!"

-- | Converts a hex number to an individual color
getColor :: Word32 -> PrimaryColor -> Word8
getColor wordColor pc = fromIntegral $ shiftR (wordColor .&. mask) sh
         where
         mask = colorMask pc
         sh = colorShift pc

-- | Separates a 32-bit rgba to a tuple of 4 8-bit colors
toColor :: Word32 -> ColorRGBA8
toColor wordColor = ColorRGBA8 (gc Red) (gc Green) (gc Blue) (gc Alpha)
        where
        gc = getColor wordColor


data PrimaryColor = Red | Green | Blue | Alpha
     deriving (Eq, Show)



luminance :: ColorRGBA -> Double             
luminance (ColorRGBA r g b _) = r * 0.2125 + g * 0.7154 + b * 0.0721


cToGrey :: ColorRGBA -> ColorRGBA
cToGrey c@(ColorRGBA _ _ _ a) = ColorRGBA lum lum lum a
           where
           lum = luminance c
        

 