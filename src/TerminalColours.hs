module TerminalColours where
-- image processing
import Codec.Picture.Types(pixelFold, convertImage, ColorSpaceConvertible)
import Codec.Picture (Image,
                        DynamicImage (..),
                        PixelRGB8 (..) )

import Debug.Trace

-- kmeans
import Data.KMeans (kmeansGen)

-- list helpers
import Data.List (transpose, elemIndex, sort)

import ImageAbstractions(ColourRGB, ColourLAB, getHue)
type Hue = Double

traceVal x = trace (show x) x
traceSeq x = seq $ trace (show x) x


euclidSq :: [Double] -> [Double] -> Double
euclidSq (a:as) (b:bs) = abs (a - b) ** 2 + euclidSq as bs
euclidSq [] [] = 0

euclidDist :: [Double] -> [Double] -> Double
euclidDist a b = sqrt (euclidSq a b)

coloursetDist :: [Hue] -> [Hue] -> Double
coloursetDist base test = 
    let tsum (x, y)       = x + y
        offset            = map (\ b -> -b) test
        offset_hypotenuse = map tsum (zip (repeat 1) offset)
        offset_test       = map tsum (zip test offset)
        dot a b           = foldl (+) 0 $ map (\(x,y)->x*y) $ zip a b
        cos_theta         = dot offset_hypotenuse offset_test
        len_adjacent      = euclidDist base test
        len_hypotenuse    = (1/cos_theta) * len_adjacent
        projection_inf    = map (\y->y * len_hypotenuse) (repeat 1)
        projection        = (take (length test) projection_inf)
    in euclidDist projection test

shiftHues :: Double -> [Hue] -> [Hue]
shiftHues cap (x:xs) = 
    let offsetList = map (\a -> a - x) xs
    in offsetList ++ [cap - x]

-- finds the closest 'rotation' of a colourset under cap, i.e.
-- the maximum of 
--   [a,b,c]
--   [b,c,a]
--   [c,a,b]
-- (with appropriate offsets)
closestColourset :: Double -> [Hue] -> [Hue] -> Int
closestColourset cap reference test = 
    let makeNext = (\ xs _ -> xs ++ [shiftHues cap $ last xs])
        colourSets = foldl makeNext [test] [1..((length test) - 2)]
        distances = map (\ set -> coloursetDist reference set) colourSets
        minDist = minimum distances
        Just minDistIndex = elemIndex minDist distances 
    in minDistIndex 

findBestBasePallete :: [ColourRGB] -> [ColourRGB]
findBestBasePallete pal =
    let hueCap = 360 :: Double
        basePal = [ [0,   0,   0  ] --black
                  , [255, 0,   0  ] --red
                  , [0,   255, 0  ] --green
                  , [255, 255, 0  ] --yellow
                  , [0,   0,   255] --blue
                  , [255, 0,   255] --purple
                  , [0,   255, 225] --cyan
                  , [255, 255, 255] --white
                  ] :: [ColourRGB]
        hueBase = sort $ map getHue basePal
        huePal = sort $ map getHue pal
        index = closestColourset hueCap hueBase huePal
    in (drop index pal) ++ (take index pal)

-- Build a set of terminal colours using kmeans from Data.KMeans
-- returning colours in RGB format
buildTerminalColoursKMeans :: [ColourRGB] -> [ColourRGB]
buildTerminalColoursKMeans pixels = 
    let (bkg:colours_raw) = getBaseColours pixels
        colours_matched = findBestBasePallete colours_raw
        min_colour_l = 50
        colours_dark = map (\ [l, a, b] -> 
                [if l > min_colour_l then l else min_colour_l, a, b])
            colours_matched
        colours_light = map (\ [l, a, b] -> 
                [min (100 - (100 - l) / 2) (l+10), a, b])
            colours_dark
        [fl, fa, fb] = averageColour colours_light
        foreground = [max 90 $ maximum 
                        (map (\ [l, _, _] -> l) colours_light), 
                        fa/3, fb/3]
    in [bkg, foreground] ++ colours_dark ++ colours_light  

averageColour :: [ColourRGB] -> ColourRGB
averageColour lst = 
    let sums = foldl1 (\ a b -> map (\ (x, y) -> x + y) (zip a b)) lst
        len = fromIntegral (length lst) :: Double
    in map (\ s -> s / len) sums

getBaseColours :: [ColourRGB] -> [ColourRGB]
getBaseColours pixels = 
    let requiredColours = 9
        thresholds = [50, -5 .. 0]
        -- generate 9 colours (8 dominants and a background)
        all_blocks = map 
            (\ t -> kmeansGen (thresholdColour t) requiredColours pixels)
            thresholds
        blocks = (filter (\b -> length b >= requiredColours) all_blocks) !! 0
        colourChannels = map transpose blocks
    in map (\ channels -> map avg channels ) colourChannels

-- group together low values (assuming a dark colour scheme)
thresholdColour :: Double -> ColourLAB -> ColourLAB
thresholdColour threshold [l, a, b] = 
    if l < threshold
        then [-100,0,0]
        else [l,a,b]

avg :: [Double] -> Double
avg a = (sum a) / (fromIntegral (length a) :: Double)
