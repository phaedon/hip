module Hip.ColorAdjust where

import Hip.ColorSpace
import Hip.Lift
import Hip.Image

luminance :: RGBAColor -> Float
luminance (r, g, b, _) = rf * 0.2125 + gf * 0.7154 + bf * 0.0721
          where
          rf = cToFloat r / 255
          gf = cToFloat g / 255 
          bf = cToFloat b / 255

greyscaleP :: RGBAColor -> RGBAColor
greyscaleP c@(_, _, _, a) = floatToRGBA (lum, lum, lum, af)
           where
           lum = luminance c
           af = cToFloat a / 255
           
greyscale :: Image -> Image
greyscale = lift1 greyscaleP