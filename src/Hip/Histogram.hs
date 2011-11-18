-- |
-- Module      : Hip.Histogram
-- Copyright   : (c) Phaedon Sinis
-- License     : BSD-style
-- 
-- Maintainer  : Phaedon Sinis <sinis AT stanford>
-- Stability   : experimental
-- 
-- Histograms are an input into some image processing algorithms,
-- most notably fast median filtering.
-- This module allows the creation and updating of histograms that 
-- are oriented around such algorithms.
module Hip.Histogram where

import Hip.ColorSpace
import Hip.PointSpace
import Hip.Image

--import qualified Data.List as DL -- in case I need foldl'
import qualified Data.Map as Map
import Data.Maybe

type HistCache = Map.Map Int CHist


columnHist :: ImageRGBA -> HistCache -> (Int, Int) -> Int -> (HistCache, CHist)
columnHist img cache (col, row) rad 
           -- Column not yet in cache? compute explicitly and add to cache
           | isNothing cacheE = (expCache, newHist)

           -- Curr row in cache? just return!
           | row == cachedRow = (cache, fromJust cacheE)

           | otherwise = (adjCache, slidDown)
           
           where
           -- Maybe CHist, by searching map for something in that column
           cacheE = Map.lookup col cache
           
           -- what's the row number of the thing retrieved from the cache?
           cachedRow = rowNum $ fromJust cacheE 

           -- Expanded cache with inserted column
           expCache = Map.insert col newHist cache

           -- New column histogram
           newHist = createColumnHist img rad col row

           -- SNEAKY recursive call here
           (cacheAbove, histAbove) = columnHist img cache (col, row - 1) rad
           slidDown = slideCol img histAbove
           -- Note use of cacheAbove here. Very careful!
           adjCache = Map.insert col slidDown cacheAbove


kernelHist :: ImageRGBA -> HistCache -> (Int, Int) -> Int -> (HistCache, KHist)
kernelHist img cache (col, row) rad = (caches, KHist redM greenM blueM)
           where
           colHists = [columnHist img cache (c, row) rad | c <- [col - rad..col + rad]]

           -- TODO: make this more accurate, more efficient?
           caches = Map.unions [m | (m, c) <- colHists]
           
           redM = Map.unionsWith (+) [redCM c | (_, c) <- colHists]
           greenM = Map.unionsWith (+) [greenCM c | (_, c) <- colHists]
           blueM = Map.unionsWith (+) [blueCM c | (_, c) <- colHists]
           

-- | Represents a single column histogram. 
-- 
data CHist = CHist {

     -- Automatically recomputed as column slides down     
     redCM :: Map.Map Int Int,
     greenCM :: Map.Map Int Int,
     blueCM :: Map.Map Int Int,

     colNum :: Int, -- ^ constant. Don't change!
     rowNum :: Int, -- ^ increment by 1 as column slides
     radius :: Int -- ^ constant
     
} deriving (Show)


-- | Represents the histogram for the entire image region of interest.
-- 
data KHist = KHist {
       redKM :: Map.Map Int Int,
       greenKM :: Map.Map Int Int,
       blueKM :: Map.Map Int Int
} deriving (Show)


-- | Slides a column histogram down by 1 row
slideCol :: ImageRGBA -> CHist -> CHist
slideCol img ch = CHist rAdj gAdj bAdj (colNum ch) (rowNum ch + 1) rad
         where

         rad = radius ch
         drad = fromIntegral rad         

         rAdj = Map.adjust (+(-1)) redSubBucket $ Map.adjust (+1) redAddBucket (redCM ch)
         gAdj = Map.adjust (+(-1)) greenSubBucket $ Map.adjust (+1) greenAddBucket (greenCM ch)
         bAdj = Map.adjust (+(-1)) blueSubBucket $ Map.adjust (+1) blueAddBucket (blueCM ch)

         x = fromIntegral $ colNum ch

         -- What's the y coordinate of the cells we're adding and subtracting?
         y_ctr = fromIntegral $ rowNum ch
         y_add = y_ctr + drad
         y_sub = y_ctr + 1 - drad
         
         -- What points are we adding & subtracting?
         addPoint = Point2d x y_add
         subPoint = Point2d x y_sub
         
         -- eval the image for those endpoints
         addColor = eval img addPoint
         subColor = eval img subPoint
                  
         -- figure out which buckets we're modifying in the existing histogram
         redAddBucket = round (255 * red addColor)
         greenAddBucket = round (255 * green addColor)
         blueAddBucket = round (255 * blue addColor)

         redSubBucket = round (255 * red subColor)
         greenSubBucket = round (255 * green subColor)
         blueSubBucket = round (255 * blue subColor)


-- | Given a Kernel Histogram in a particular state (i.e. 
-- with the current column histograms already merged), compute
-- the median color over that region
kMedian :: KHist -> ColorRGBA
kMedian khist = ColorRGBA r g b 1
        where
        r = mMedian (redKM khist) / 255
        g = mMedian (greenKM khist) / 255
        b = mMedian (blueKM khist) / 255


mMedianRec :: Map.Map Int Int -> [Int] -> Int -> Int -> Int
mMedianRec m keys acc lim
           | Map.null m = 0  -- ugh. Just for testing I guess.
           | acc + freq < lim = mMedianRec m xs (acc + freq) lim
           | null xs = x
           | otherwise = x
           where 
           freq = fromJust $ Map.lookup x m
           (x:xs) = keys


mMedian :: Map.Map Int Int -> Double
mMedian m = fromIntegral $ mMedianRec m keys 0 lim
        where
        keys = Map.keys m

        numElems = Map.fold (+) 0 m
        rad = numElems `div` 2
        
        lim = rad + 1


genColCoords :: Int -> Int -> Int -> [Point2d]
genColCoords kradius col row = [createPoint (col, y) | y <- [row - kradius..row+kradius]]


valsToHMap :: [Int] -> Map.Map Int Int
valsToHMap valList = foldr (myInsert 1) Map.empty valList

           where
           myInsert :: Int -> Int -> Map.Map Int Int -> Map.Map Int Int
           myInsert v k = Map.insertWith (+) k v


createColumnHist :: ImageRGBA -> Int -> Int -> Int -> CHist
createColumnHist img kradius col row

                 -- histogram!
                 = CHist rv gv bv col row kradius

                 where
                 
                 -- a list of the points we care about
                 points = genColCoords kradius col row

                 -- vectors of RGB color values
                 redvals = [round $ 255 * (red $ eval img pt) | pt <- points]
                 greenvals = [round $ 255 * (green $ eval img pt) | pt <- points]
                 bluevals = [round $ 255 * (blue $ eval img pt) | pt <- points]

                 -- frequency histograms of each color channel
                 rv = valsToHMap redvals
                 gv = valsToHMap greenvals
                 bv = valsToHMap bluevals 



                 

-- $references
--
-- * Perreault, S., & Hebert, P. (2007). Median filtering in constant time. 
--   IEEE transactions on image processing : a publication of the IEEE Signal 
--   Processing Society, 16(9), 2389-94. <http://nomis80.org/ctmf.html>

