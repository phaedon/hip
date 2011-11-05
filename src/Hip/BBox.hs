module Hip.BBox where


data BBox = Infinite
     | BBox {
       corner :: (Double, Double),
       width :: Double,
       height :: Double
       } deriving (Show, Eq)
         