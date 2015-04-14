module Main where

import Data.List (foldl', intercalate)

-- Simple stuff for IO
import System.IO (stdin)
import System.Exit (exitWith, ExitCode(..))
import System.Environment (getArgs)
import Data.ByteString (hGetContents)
import Data.String.Utils(split, join)
import System.Console.GetOpt (
    getOpt',
    OptDescr(..),
    ArgDescr(..),
    ArgOrder(Permute))

-- codec imports
import Codec.Picture.Types(pixelFold, convertImage, ColorSpaceConvertible)
import Codec.Picture (decodeImage,
                        readImage,
                        DynamicImage (..))

-- My Modules
import TerminalColours (buildTerminalColoursKMeans)
import ImageAbstractions (processDynamicImage, convertToRGB, convertToLAB)
import IOHelpers (showColoursXGCM, showColoursXResources)



data MaybeFlag = Error String | Flag FLAG

-- options
data FLAG = 
      THEME_LIGHT
    | THEME_DARK

    | METHOD_KMEANS
    deriving (Show)

-- opt definitions
background_opt =
    Option ['t'] ["theme"]
        (ReqArg mapping "LIGHTNESS")
        "theme LIGHTNESS (light/dark)"
    where mapping =
            (\arg -> case arg of
                "light" -> Flag THEME_LIGHT
                "dark"  -> Flag THEME_DARK
                u       -> Error ("Unknown Theme " ++ show u))

method_opt =
    Option ['m'] ["method"]
        (ReqArg mapping "METHOD")
        "generation METHOD (kmeans/..)"
    where mapping =
            (\arg -> case arg of
                "kmeans" -> Flag METHOD_KMEANS
                u        -> Error ("Unknown method " ++ show u))

options :: [OptDescr MaybeFlag]
options =
    [ background_opt
    , method_opt ]

getOpt :: [String] -> Either String ([FLAG], [String])
getOpt argv =
    let (flags, nonopts, unknownopts, errs) = getOpt' Permute options argv
        flagErrors = filter 
            (\ f -> case f of 
                Error _ -> True
                _       -> False) flags
        
    in if length flagErrors > 0
        then let flagErrorMessages = map (\ (Error s) -> s) flagErrors
            in Left ("Error in parsing flags:\n\t"
                        ++ (intercalate "\n\t" flagErrorMessages))
        else if length errs > 0
            then Left ("Error parsing argv\n" ++ (foldl' (++) "" errs))
            else let realFlags = map (\ (Flag f) -> f) flags
                in Right (realFlags, nonopts)

-- split path on image decoding error
either' switch fail succeed = either fail succeed switch

imgSuccess :: DynamicImage -> IO()
imgSuccess img = do
    -- putStrLn $ getColourSpaceName img
    let imgPixelsRGB = processDynamicImage img
        imgPixelsLAB = map convertToLAB imgPixelsRGB
        coloursLAB = buildTerminalColoursKMeans imgPixelsLAB
        coloursRGB = map convertToRGB coloursLAB
    putStrLn $ showColoursXGCM $ coloursRGB

exitWithError err = do
    putStrLn err
    exitWith (ExitFailure 1)

imgFailure :: String -> IO()
imgFailure msg = exitWithError $ "Error decoding image:\n" ++ msg

main :: IO()
main = do
    argv <- getArgs
    either' (getOpt argv) exitWithError (\flags ->
        do
        -- im <- readImage "images/dock_tiny.jpg"
        imStream <- hGetContents stdin
        let im = decodeImage imStream
        either' im imgFailure imgSuccess)
