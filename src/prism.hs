module Main where

-- Simple stuff for IO
import System.IO (stdin)
import Data.ByteString (hGetContents)
import Data.String.Utils(split, join)
import System.Exit (exitWith, ExitCode(..))
import Numeric (showHex)

-- codec imports
import Codec.Picture.Types(pixelFold, convertImage, ColorSpaceConvertible)
import Codec.Picture (decodeImage,
                        readImage,
                        Image,
                        DynamicImage (..),
                        PixelRGB8 (..) )

-- colour conversions
import Data.Prizm.Color.CIE.LAB(fromRGB, toRGB)
import Data.Prizm.Types(CIELAB(CIELAB), RGB(RGB))

-- Terminal Colours Module
import TerminalColours (buildTerminalColours)

-- list helpers
import Data.List (foldl')




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




-- Colour Space Conversions
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
rgbToHexCode channels = foldl' (++) "#" hexes
    where   intRGB = (map floor channels)
            hexesShowS = map showHex intRGB
            shortHexes = map (\ showS -> showS "") hexesShowS
            hexes = map (\ x -> if length x < 2
                                    then "0" ++ x
                                    else x) shortHexes

showSequenceXResources :: (Show a) => [String] -> Int -> [a] -> String
showSequenceXResources _ _ [] = ""
showSequenceXResources (name:names) index (x:xs) = 
    name 
        ++ ":\t" 
        ++ (show x) 
        ++ "\n" 
        ++ (showSequenceXResources names (index + 1) xs)

showColoursXResources colours =
    let colourNamer = 
            (\ index -> "*." ++ case index of 
                0 -> "background"
                1 -> "foreground"
                _ -> (++) "color" $ show $ index - 2)        
        colourNames = map colourNamer [0..]
    in showSequenceXResources colourNames 0 colours

showSequenceINI :: (Show a) => [String] -> Int -> [a] -> String 
showSequenceINI _ _ [] = ""
showSequenceINI (name:names) index (x:xs) =
    "\t"
        ++ name
        ++ " = "
        ++ (show x)
        ++ "\n"
        ++ (showSequenceINI names (index + 1) xs)

showColoursXGCM colours =
    let names = ["foreground"
                , "background"
                , "black"
                , "bright_black"
                , "red"
                , "bright_red"
                , "green"
                , "bright_green"
                , "yellow"
                , "bright_yellow"
                , "blue"
                , "bright_blue"
                , "magenta"
                , "bright_magenta"
                , "cyan"
                , "bright_cyan"
                , "white"
                , "bright_white"]
    in "[attributes]\n" ++ showSequenceINI names 0 colours

imgSuccess :: DynamicImage -> IO()
imgSuccess img = do
            -- putStrLn $ getColourSpaceName img
            let imgPixelsRGB = convertImageToDoubleList img
                imgPixelsLAB = map convertToLAB imgPixelsRGB
                coloursLAB = buildTerminalColours imgPixelsLAB
                coloursRGB = map convertToRGB coloursLAB
            putStrLn $ showColoursXGCM $ map rgbToHexCode coloursRGB

imgFailure :: String -> IO()
imgFailure msg = do
            exitWithError $ "Error decoding image:\n" ++ msg

main :: IO()
main = do
    -- im <- readImage "images/dock_tiny.jpg"
    imStream <- hGetContents stdin
    let im = decodeImage imStream
    either imgFailure imgSuccess im
