module Main where
import System.Exit (exitWith, ExitCode(..))

import Data.List (transpose)
import Data.KMeans (kmeansGen)

import Numeric (showHex)
import Codec.Picture (decodeImage,
                        Image,
                        DynamicImage (..),
                        PixelRGB8 (..) )
import Codec.Picture.Types(pixelFold, convertImage, ColorSpaceConvertible)
import System.IO (stdin)
import Data.ByteString (hGetContents)

import Data.Prizm.Color.CIE.LAB(fromRGB, toRGB)
import Data.Prizm.Types(CIELAB(CIELAB), RGB(RGB))



-- Folds an image into a list of doubles so it can be consumed by kheap
collapseToDoubleList :: [[Double]] -> Int -> Int -> PixelRGB8 -> [[Double]]
collapseToDoubleList lst x y (PixelRGB8 r g b) =
    let words = [r, g, b]
        in lst ++ [map (\ word -> fromIntegral word :: Double) words]

convertImageToDoubleList :: DynamicImage -> [[Double]]
convertImageToDoubleList img =
    let c img = pixelFold collapseToDoubleList []  img
        ci = convertImage
    in case img of
        ImageRGB8   i   -> c i
        ImageYCbCr8 i   -> c (ci i :: Image PixelRGB8)
        _ -> [[0.0]]


-- Build a set of terminal colours using kmeans from Data.KMeans
-- returning colours in RGB format
buildTerminalColours :: [[Double]] -> [[Double]]
buildTerminalColours pixels = 
    let (bkg:colours_raw) = getBaseColours pixels
        min_colour_l = 50
        colours_dark = map (\ [l, a, b] -> 
                [if l > min_colour_l then l else min_colour_l, a, b])
            colours_raw
        colours_light = map (\ [l, a, b] -> 
                [min (100 - (100 - l) / 2) (l+10), a, b])
            colours_dark
        [fl, fa, fb] = averageColour colours_light
        foreground = [max 90 $ maximum (map (\ [l, _, _] -> l) colours_light), fa/3, fb/3]

    in [bkg, foreground] ++ colours_dark ++ colours_light  

averageColour :: [[Double]] -> [Double]
averageColour lst = 
    let sums = foldl1 (\ a b -> map (\ (x, y) -> x + y) (zip a b)) lst
        len = fromIntegral (length lst) :: Double
    in map (\ s -> s / len) sums

getBaseColours :: [[Double]] -> [[Double]]
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
thresholdColour :: Double -> [Double] -> [Double]
thresholdColour threshold [l, a, b] = 
    if l < threshold
        then [-100,0,0]
        else [l,a,b]

avg :: [Double] -> Double
avg a = (sum a) / (fromIntegral (length a) :: Double)


-- Helper functions for input and output
getColourSpaceName :: DynamicImage -> String
getColourSpaceName img = case img of
    ImageY8 _       -> "ImageY8"
    ImageYA8 _      -> "ImageYA8"
    ImageRGB8 _     -> "ImageRGB8"
    ImageRGBA8 _    -> "ImageRGBA8"
    ImageYCbCr8 _   -> "ImageYCbCr8"
    _ -> "Unknown"


exitWithError :: String -> IO a
exitWithError err = do putStrLn err
                       exitWith (ExitFailure 1)

rgbToHexCode :: [Double] -> String
rgbToHexCode channels = foldl (++) "#" hexes
    where   intRGB = (map floor channels)
            hexesShowS = map showHex intRGB
            shortHexes = map (\ showS -> showS "") hexesShowS
            hexes = map (\ x -> if length x < 2
                                    then "0" ++ x
                                    else x) shortHexes

toDouble :: Integral a => a -> Double
toDouble i = fromIntegral i :: Double

convertToLAB :: [Double] -> [Double]
convertToLAB [r, g, b] = let
        rgb :: RGB Integer
        rgb = (RGB (floor r) (floor g) (floor b))
        lab :: CIELAB Double
        lab = fromRGB rgb
        CIELAB _l _a _b = lab
    in [_l, _a, _b]

convertToRGB :: [Double] -> [Double]
convertToRGB [l, a, b] = let 
        lab  :: CIELAB Double
        lab = (CIELAB l a b)
        rgb  :: RGB Integer
        rgb = toRGB lab
        RGB _r _g _b = rgb
    in map toDouble [_r, _g, _b]

main :: IO()
main = do
    putStrLn "reading ImageRGB8e"
    imstream <- hGetContents stdin
    putStrLn "reading complete, passing to kmeans"
    either failure success $! decodeImage imstream
    where 
        success img = do
            putStrLn $ "image colour space: " ++ getColourSpaceName img
            let imgPixelsRGB = convertImageToDoubleList img
                imgPixelsLAB = map convertToLAB imgPixelsRGB
                coloursLAB = buildTerminalColours imgPixelsLAB
                coloursRGB = map convertToRGB coloursLAB
            putStrLn $ show coloursRGB
            putStrLn $ show $ map rgbToHexCode coloursRGB
        failure msg = do
            exitWithError $ "Error decoding image:\n" ++ msg
