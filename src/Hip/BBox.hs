module Hip.BBox where



import Hip.PointSpace

data BBox2d = BBox2d { corner :: Point2d,
                       width :: Double,
                       height :: Double } deriving (Show)

-- | The emp†y bounding box has no extent
emptyBBox :: BBox2d
emptyBBox = BBox2d pointOrigin 0 0

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


upperRight :: BBox2d -> Point2d
upperRight (BBox2d (Point2d x y) w h) = Point2d (x + w) (y + h)


isOverlapping :: BBox2d -> BBox2d -> Bool
isOverlapping bb1@(BBox2d (Point2d x1 y1) _ _) bb2@(BBox2d (Point2d x2 y2) _ _) 
              -- one is above the other?
              | ury1 < y2 || ury2 < y1 = False
              -- one is to the left of the other?
              | urx1 < x2 || urx2 < x1 = False
              | otherwise = True
              where
              (Point2d urx1 ury1) = upperRight bb1
              (Point2d urx2 ury2) = upperRight bb2
              
              

bbUnion :: BBox2d -> BBox2d -> BBox2d
bbUnion (BBox2d (Point2d x1 y1) w1 h1) (BBox2d (Point2d x2 y2) w2 h2) 
            = BBox2d (Point2d lowx lowy) bigw bigh
            where
            lowx = min x1 x2
            lowy = min y1 y2
            highx = max (x1 + w1) (x2 + w2)
            highy = max (y1 + h1) (y2 + h2)
            bigw = highx - lowx
            bigh = highy - lowy
            

bbIntersection :: BBox2d -> BBox2d -> BBox2d
bbIntersection bb1@(BBox2d (Point2d x1 y1) w1 h1) bb2@(BBox2d (Point2d x2 y2) w2 h2) 
               | not (isOverlapping bb1 bb2) = emptyBBox
               | otherwise = BBox2d (Point2d lowx lowy) smallw smallh
               where
               lowx = max x1 x2
               lowy = max y1 y2
               highx = min (x1 + w1) (x2 + w2)
               highy = min (y1 + h1) (y2 + h2)
               smallw = highx - lowx
               smallh = highy - lowy