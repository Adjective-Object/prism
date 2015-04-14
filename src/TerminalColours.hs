module TerminalColours where
-- image processing
import Codec.Picture.Types(pixelFold, convertImage, ColorSpaceConvertible)
import Codec.Picture (Image,
                        DynamicImage (..),
                        PixelRGB8 (..) )

-- kmeans
import Data.KMeans (kmeansGen)

-- colour conversions
import Data.Prizm.Color.CIE.LAB(fromRGB, toRGB)
import Data.Prizm.Types(CIELAB(CIELAB), RGB(RGB))

-- list helpers
import Data.List (transpose)

import ImageAbstractions(ColourRGB, ColourLAB)



-- Build a set of terminal colours using kmeans from Data.KMeans
-- returning colours in RGB format
buildTerminalColoursKMeans :: [ColourRGB] -> [ColourRGB]
buildTerminalColoursKMeans pixels = 
    let (bkg:colours_raw) = getBaseColours pixels
        min_colour_l = 50
        colours_dark = map (\ [l, a, b] -> 
                [if l > min_colour_l then l else min_colour_l, a, b])
            colours_raw
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
