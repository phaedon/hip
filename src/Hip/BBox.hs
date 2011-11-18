module Hip.BBox where



import Hip.PointSpace

data BBox2d = BBox2d { corner :: Point2d,
                       width :: Double,
                       height :: Double } deriving (Show)


bboxIntDims :: BBox2d -> (Int, Int)
bboxIntDims (BBox2d _ dw dh) = (round dw, round dh)

bufferSize :: BBox2d -> Int
bufferSize (BBox2d _ dw dh)
           = (round dw * round dh) * 4

isInside :: BBox2d -> Point2d -> Bool
isInside (BBox2d (Point2d cx cy) w h) (Point2d x y) 
         | x  < cx || x > cx + w || y < cy || y > cy + h = False
         | otherwise = True

bboxToCoordList :: BBox2d -> [Point2d]
bboxToCoordList (BBox2d (Point2d cx cy) w h) = coordList
                where
                icx = round cx ::Int
                icy = round cy ::Int
                iw = round w - 1
                ih = round h - 1

                coordList = [ Point2d (fromIntegral kx) (fromIntegral ky) | kx <- [icx..(icx + iw)], ky <- [icy..(icy + ih)] ]
