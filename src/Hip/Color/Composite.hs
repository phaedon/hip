module Hip.Color.Composite where

{--
Sources:
========
- Compositing Digital Images. Porter, T., & Duff, T. (1984). 
  Computer Graphics. Computer, 18(3), 253-259.
- Functional Image Synthesis. Elliott, C. 

--}

class CompositeColor repr where

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

