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

import Data.List
import qualified Data.Vector.Unboxed as VU

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
           
           blank = VU.replicate (256::Int) (0::Int)

           redM = foldl' mergeVectors blank [redH c | (m, c) <- colHists]
           greenM = foldl' mergeVectors blank [greenH c | (m, c) <- colHists]
           blueM = foldl' mergeVectors blank [blueH c | (m, c) <- colHists]
           

-- | Represents a single column histogram. 
-- 
data CHist = CHist {

     -- Automatically recomputed as column slides down     
     redH :: VU.Vector Int,
     greenH :: VU.Vector Int,
     blueH :: VU.Vector Int,

     colNum :: Int, -- ^ constant. Don't change!
     rowNum :: Int, -- ^ increment by 1 as column slides
     radius :: Int -- ^ constant
     
} deriving (Show)


-- | Represents the histogram for the entire image region of interest.
-- 
data KHist = KHist {

     redMH :: VU.Vector Int,
     greenMH :: VU.Vector Int,
     blueMH :: VU.Vector Int

} deriving (Show)


-- | Slides a column histogram down by 1 row
slideCol :: ImageRGBA -> CHist -> CHist
slideCol img ch = CHist rAdj gAdj bAdj (colNum ch) (rowNum ch + 1) rad
         where

         rad = radius ch
         drad = fromIntegral rad         

         rAdj = mergeVectors rSub $ mergeVectors rAdd (redH ch)
         gAdj = mergeVectors gSub $ mergeVectors gAdd (greenH ch)
         bAdj = mergeVectors bSub $ mergeVectors bAdd (blueH ch)

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

         -- generate vectors of (0, 1, -1) so that we can just add
         -- TODO: this is really inefficient stuff. 
         rAdd = VU.generate 256 (\a -> if a == redAddBucket then 1 else 0)
         gAdd = VU.generate 256 (\a -> if a == greenAddBucket then 1 else 0)
         bAdd = VU.generate 256 (\a -> if a == blueAddBucket then 1 else 0)

         rSub = VU.generate 256 (\a -> if a == redSubBucket then (-1) else 0)
         gSub = VU.generate 256 (\a -> if a == greenSubBucket then (-1) else 0)
         bSub = VU.generate 256 (\a -> if a == blueSubBucket then (-1) else 0)



-- | Given a Kernel Histogram in a particular state (i.e. 
-- with the current column histograms already merged, compute
-- the median color over that region
kMedian :: KHist -> ColorRGBA
kMedian khist = ColorRGBA r g b 1
        where
        r = vMedian (VU.toList $ redMH khist) / 255
        g = vMedian (VU.toList $ greenMH khist) / 255
        b = vMedian (VU.toList $ blueMH khist) / 255

-- | Recursive computation of median
vMedianRec :: Int -> Int -> Int -> [Int] -> Int
vMedianRec counter acc lim (x:xs) 
           | acc + x < lim = vMedianRec (counter+1) (acc + x) lim xs
           | otherwise = counter
vMedianRec counter _ _ _ = counter

-- | Computes the median *bucket* from a histogram (list of ints)
vMedian :: [Int] -> Double
vMedian vec = fromIntegral $ vMedianRec 0 0 lim vec
        where
        rad = sum vec `div` 2
        lim = rad + 1

-- | For computing a histogram union (i.e. merging column histograms)
mergeVectors :: (Num c, VU.Unbox c) => VU.Vector c -> VU.Vector c -> VU.Vector c
mergeVectors = VU.zipWith (+)


genColCoords :: Int -> Int -> Int -> [Point2d]
genColCoords kradius col row = [createPoint (col, y) | y <- [row - kradius..row+kradius]]

valsToHist :: VU.Vector Double -> VU.Vector Int -> Int ->VU.Vector Int
valsToHist vals hist bucket
           | bucket == 256 = hist
           | otherwise = valsToHist remVals (VU.snoc hist freq) (bucket + 1)
           where 

           dBucket = fromIntegral bucket

           lowerLim = dBucket / 256

           upperLim | bucket == 255 = 1.0000001 -- just something to capture the upper limit
                    | otherwise = (dBucket + 1) / 256

           -- a tiny predicate function
           isInBucket val = val >= lowerLim && val < upperLim

           -- partition the list into two parts
           (inbucket, remVals) = VU.partition isInBucket vals

           -- how many?
           freq = VU.length inbucket


createColumnHist :: ImageRGBA -> Int -> Int -> Int -> CHist
createColumnHist img kradius col row

                 -- histogram!
                 = CHist rv gv bv col row kradius

                 where
                 
                 -- a list of the points we care about
                 points = genColCoords kradius col row

                 -- vectors of RGB color values
                 redvals = VU.fromList $ sort [red $ eval img pt | pt <- points]
                 greenvals = VU.fromList $ sort [green $ eval img pt | pt <- points]
                 bluevals = VU.fromList $ sort [blue $ eval img pt | pt <- points]

                 -- frequency histograms of each color channel
                 rv = valsToHist redvals VU.empty 0
                 gv = valsToHist greenvals VU.empty 0
                 bv = valsToHist bluevals VU.empty 0



                 

-- $references
--
-- * Perreault, S., & Hebert, P. (2007). Median filtering in constant time. 
--   IEEE transactions on image processing : a publication of the IEEE Signal 
--   Processing Society, 16(9), 2389-94. <http://nomis80.org/ctmf.html>

