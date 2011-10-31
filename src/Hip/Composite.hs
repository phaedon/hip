module Hip.Composite where

import Hip.Image
import Hip.Lift
import Hip.ColorSpace

{--
Sources:
========
- Compositing Digital Images. Porter, T., & Duff, T. (1984). 
  Computer Graphics. Computer, 18(3), 253-259.
- Functional Image Synthesis. Elliott, C. 

--}

------------------------------
-- COMPOSITING OPS ON IMAGES
------------------------------
pdOver, pdIn, pdOut :: Image -> Image -> Image
pdAtop, pdXor, pdPlus :: Image -> Image -> Image
pdOver = lift2 overP
pdIn = lift2 inP
pdOut = lift2 outP
pdAtop = lift2 atopP
pdXor = lift2 xorP
pdPlus = lift2 cAdd

-- TODO argh
{-
pdDarken, pdDissolve, pdOpaque :: Float -> Image -> Image
pdDarken phi = lift1 $ darkenP phi
pdDissolve delta = lift1 $ dissolveP delta
pdOpaque omega = lift1 $ opaqueP omega
-}

------------------------------
-- pixel-wise compositing ops
------------------------------

-- to be revised, depending on representation of an image
overP :: RGBAColor -> RGBAColor -> RGBAColor
overP p1 p2 = cAdd p1 scaledp2
      where 
      scaledp2 = cScale (1 - a1) p2
      (_, _, _, a1) = rgbaToFloat p1 

inP :: RGBAColor -> RGBAColor -> RGBAColor 
inP p1 p2 = cScale a2 p1
    where
    (_, _, _, a2) = rgbaToFloat p2 

outP :: RGBAColor -> RGBAColor -> RGBAColor 
outP p1 p2 = cScale (1 - a2) p1
     where 
     (_, _, _, a2) = rgbaToFloat p2 

atopP :: RGBAColor -> RGBAColor -> RGBAColor
atopP p1 p2 = cAdd scaledc1 scaledc2
      where 
      (_, _, _, a1) = rgbaToFloat p1 
      (_, _, _, a2) = rgbaToFloat p2 
      scaledc1 = cScale a2 p1
      scaledc2 = cScale (1 - a1) p2

xorP :: RGBAColor -> RGBAColor -> RGBAColor
xorP p1 p2 = cAdd scaledc1 scaledc2
      where 
      (_, _, _, a1) = rgbaToFloat p1 
      (_, _, _, a2) = rgbaToFloat p2 
      scaledc1 = cScale (1 - a2) p1
      scaledc2 = cScale (1 - a1) p2

-- A unary pointwise op that creates a closure: pointwise multiplication with a constant vector
unaryP :: FloatColor -> FloatColor -> FloatColor
unaryP v = flCMult v

darkenP :: Float -> FloatColor -> RGBAColor
darkenP phi flCol = floatToRGBA $ unaryP (phi, phi, phi, 1) flCol

dissolveP :: Float -> FloatColor -> RGBAColor
dissolveP delta fl = floatToRGBA $ flScale delta fl

opaqueP :: Float -> FloatColor -> RGBAColor 
opaqueP omega flCol = floatToRGBA $ unaryP (1, 1, 1, omega) flCol
