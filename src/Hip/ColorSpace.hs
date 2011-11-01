module Hip.ColorSpace where

import Data.Word
import Data.Bits

data PrimaryColor = Red | Green | Blue | Alpha
     deriving (Eq, Show)

class Color a where
      -- Saturating addition
      (++) :: a -> a -> a

      -- Saturating pointwise multiplication
      (*.) :: a -> a -> a

      -- Scalar multiplication
--      (**) :: Double -> a -> a

type Channel8 = Word8
type RGBAColor = (Word8, Word8, Word8, Word8)
type RGBColor = (Word8, Word8, Word8)

-- | values must be in [0, 1]:
type FloatColor = (Float, Float, Float, Float)



boolToRGBA :: Bool -> RGBAColor
boolToRGBA b | b = (255, 255, 255, 255)
             | otherwise = (0, 0, 0, 255)

rgbaToBool :: Float -> RGBAColor -> Bool
rgbaToBool threshold rgba | lum < threshold = False
                          | otherwise = True
           where
           lum = luminance rgba

rgbaToFloat :: RGBAColor -> FloatColor
rgbaToFloat (r, g, b, a) = (rf, gf, bf, af)
            where 
            rf = (cToFloat r / 255)
            gf = (cToFloat g / 255)
            bf = (cToFloat b / 255)
            af = (cToFloat a / 255)

floatToRGBA :: FloatColor -> RGBAColor
floatToRGBA (rf, gf, bf, af) = (r, g, b, a)
            where
            r = round $ rf * 255
            g = round $ gf * 255
            b = round $ bf * 255
            a = round $ af * 255

cToInt :: Word8 -> Int
cToInt c = fromIntegral c

cToFloat :: Word8 -> Float
cToFloat c = fromIntegral c

-- | Color accessors:
red, green, blue, alpha :: RGBAColor -> Word8
red (r, _, _, _) = r
green (_, g, _, _) = g
blue (_, _, b, _) = b
alpha (_, _, _, a) = a

flCMult :: FloatColor -> FloatColor -> FloatColor
flCMult (rf1, gf1, bf1, af1) (rf2, gf2, bf2, af2) = 
        (satScale rf1 rf2,
        satScale gf1 gf2,
        satScale bf1 bf2,
        satScale af1 af2)

flCAdd :: FloatColor -> FloatColor -> FloatColor
flCAdd (rf1, gf1, bf1, af1) (rf2, gf2, bf2, af2) = 
       (rf1 + rf2, gf1 + gf2, bf1 + bf2, af1 + af2)

-- | 
satAdd :: Word8 -> Word8 -> Word8
satAdd n1 n2 | (cToInt n1) + (cToInt n2) >= 256 = 255
             | otherwise = n1 + n2

-- | Preserves range in [0, 1]
satScale :: Float -> Float -> Float
satScale s n | sn <= 0 = 0
             | sn > 1 = 1
             | otherwise = sn
         where sn = s * n

-- | Saturating addition on colors
cAdd :: RGBAColor -> RGBAColor -> RGBAColor
cAdd (r1, g1, b1, a1) (r2, g2, b2, a2) = (r3, g3, b3, a3)
    where 
    r3 = satAdd r1 r2
    g3 = satAdd g1 g2
    b3 = satAdd b1 b2
    a3 = satAdd a1 a2        


-- | Scale color by a constant factor.
flScale :: Float -> FloatColor -> FloatColor
flScale s (rf, gf, bf, af) = (ss rf, ss gf, ss bf, ss af)
       where
       ss = satScale s

cScale :: Float -> RGBAColor -> RGBAColor
cScale s col = floatToRGBA $ flScale s (rgbaToFloat col)


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
toColor :: Word32 -> RGBAColor
toColor wordColor = (gc Red, gc Green, gc Blue, gc Alpha)
        where
        gc = getColor wordColor

-- | Converts a 4-tuple of 8-bit colors to a single 32-bit word
fromColor :: RGBAColor -> Word32
fromColor (r, g, b, a) = 
          fromIntegral (shiftL r 0)
          .|. fromIntegral (shiftL g 8)
          .|. fromIntegral (shiftL b 16)
          .|. fromIntegral (shiftL a 24)

-- | Converts a color to a luminance value in [0, 1]
--   (Source: GLSL orange book)
luminance :: RGBAColor -> Float
luminance (r, g, b, _) = rf * 0.2125 + gf * 0.7154 + bf * 0.0721
          where
          rf = cToFloat r / 255
          gf = cToFloat g / 255 
          bf = cToFloat b / 255
