module IOHelpers where

import ImageAbstractions (ColourRGB)
import Codec.Picture (DynamicImage (..))
import Data.List (foldl')
import Numeric (showHex)




getColourSpaceName :: DynamicImage -> String
getColourSpaceName img = case img of
    ImageY8 _       -> "ImageY8"
    ImageYA8 _      -> "ImageYA8"
    ImageRGB8 _     -> "ImageRGB8"
    ImageRGBA8 _    -> "ImageRGBA8"
    ImageYCbCr8 _   -> "ImageYCbCr8"
    _ -> "Unknown"

rgbToHexCode :: ColourRGB -> String
rgbToHexCode channels = foldl' (++) "#" hexes
    where   intRGB = (map floor channels)
            hexesShowS = map showHex intRGB
            shortHexes = map (\ showS -> showS "") hexesShowS
            make2Digits = (\ x -> if length x < 2 then "0" ++ x else x)
            hexes = map make2Digits shortHexes

showSequenceXResources :: (Show a) => [String] -> Int -> [a] -> String
showSequenceXResources _ _ [] = ""
showSequenceXResources (name:names) index (x:xs) = 
    name 
        ++ ":\t" 
        ++ (show x) 
        ++ "\n" 
        ++ (showSequenceXResources names (index + 1) xs)

showColoursXResources :: [ColourRGB] -> String
showColoursXResources colours =
    let colourNamer = 
            (\ index -> "*." ++ case index of 
                0 -> "foreground"
                1 -> "background"
                _ -> (++) "color" $ show $ index - 2)        
        colourNames = map colourNamer [0..]
    in showSequenceXResources colourNames 0 (map rgbToHexCode colours)

showSequenceINI :: (Show a) => [String] -> Int -> [a] -> String 
showSequenceINI _ _ [] = ""
showSequenceINI (name:names) index (x:xs) =
    "\t"
        ++ name
        ++ " = "
        ++ (show x)
        ++ "\n"
        ++ (showSequenceINI names (index + 1) xs)

showColoursXGCM :: String -> [ColourRGB] -> String
showColoursXGCM imgpath colours =
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
    in "[attributes]\n" 
        ++ (if imgpath /= "" then "\tdesktop = \"" ++ imgpath ++"\"\n" else "")
        ++ showSequenceINI names 0 (map rgbToHexCode colours)
