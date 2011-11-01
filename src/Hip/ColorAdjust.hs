module Hip.ColorAdjust where

import Hip.ColorSpace
import Hip.Lift
import Hip.Image


greyscaleP :: RGBAColor -> RGBAColor
greyscaleP c@(_, _, _, a) = floatToRGBA (lum, lum, lum, af)
           where
           lum = luminance c
           af = cToFloat a / 255
           
greyscale :: Image -> Image
greyscale = lift1 greyscaleP

binary :: Float -> Image -> Image
binary th = binaryToRGBAImage . (rgbaToBinaryImage th)

rgbaToBinaryImage :: Float -> Image -> BinaryImage
rgbaToBinaryImage thresh = lift1 (rgbaToBool thresh)

binaryToRGBAImage :: BinaryImage -> Image
binaryToRGBAImage = lift1 boolToRGBA