module Main where

import Data.List (foldl', intercalate, repeat, intersect)

-- Simple stuff for IO
import System.IO (stdin)
import System.Exit (exitWith, ExitCode(..))
import System.Environment (getProgName, getArgs)
import Data.ByteString (hGetContents)
import Data.String.Utils(split, join)
import System.Console.GetOpt (
    getOpt',
    OptDescr(..),
    ArgDescr(..),
    ArgOrder(Permute))

-- codec imports
import Codec.Picture.Types(
    pixelFold,
    convertImage,
    ColorSpaceConvertible)
import Codec.Picture (
    decodeImage,
    readImage,
    DynamicImage (..))

-- My Modules
import TerminalColours (buildTerminalColoursKMeans)
import ImageAbstractions (
    processDynamicImage,
    convertToRGB,
    convertToLAB,
    ColourRGB)
import IOHelpers (showColoursXGCM, showColoursXResources)


data MaybeFlag = Error String | Flag FLAG

-- options
data FLAG = HELP
    | THEME_LIGHT
    | THEME_DARK

    | METHOD_KMEANS

    | FORMAT_XRESOURCES
    | FORMAT_XGCM
    deriving (Show, Eq)

(+\) a b = a ++ "\n" ++ b
(+\\) a b = a ++ "\n    " ++ b

-- help text
helpText :: String -> String
helpText progName =
    "usage: " ++ progName ++ " [options] [imagePath]" +\
    "imagePath can be either a path to an image or \"-\","
        ++ "to read from stdin" +\
    "[options] one of" +\\
        ( intercalate "\n    " optionStrings)

optionStrings :: [String]
optionStrings =
    let pairs =  map (\ (Option shorts longs _ desc) ->
                    let fnames = map (\c -> ['-',c]) shorts
                              ++ map (\l -> "--" ++ l) longs
                    in (intercalate ", " fnames, desc)) options
        maxflagstrlen = maximum $ map (\ (flagstr, _) -> length flagstr) pairs
    in map (\ (flagstr, desc) ->
        flagstr
        ++ take (8 + maxflagstrlen - length flagstr) (repeat ' ')
        ++ desc) pairs


-- opt definitions
help_opt =
    Option ['h'] ["help"]
        (NoArg (Flag HELP))
        "display helptext and exit"

background_opt =
    Option ['t'] ["theme"]
        (ReqArg mapping "THEME")
        "theme lightness (light/dark)"
    where mapping =
            (\arg -> case arg of
                "light" -> Flag THEME_LIGHT
                "dark"  -> Flag THEME_DARK
                u       -> Error ("Unknown Theme " ++ show u))

method_opt =
    Option ['m'] ["method"]
        (ReqArg mapping "METHOD")
        "generation method (kmeans/..)"
    where mapping =
            (\arg -> case arg of
                "kmeans" -> Flag METHOD_KMEANS
                u        -> Error ("Unknown method " ++ show u))

format_opt =
    Option ['o', 'f'] ["output", "format"]
        (ReqArg mapping "FORMAT")
        "output format (xresources/xgcm)"
    where mapping =
            (\arg -> case arg of
                "xresources" -> Flag FORMAT_XRESOURCES
                "xgcm"       -> Flag FORMAT_XGCM
                u            -> Error ("Unknown format " ++ show u))

options :: [OptDescr MaybeFlag]
options =
    [ help_opt
    , background_opt
    , method_opt
    , format_opt ]


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


-- this is where switching on flags actually happens
switchFlags :: [FLAG] -> (FLAG -> a) -> [FLAG] -> a
switchFlags relevantFlags mapping flags = 
    let rf = relevantFlags `intersect` flags
    in if length rf == 0
        then mapping $ relevantFlags !! 0 -- default
        else mapping $ rf !! 0

getBuilderFromFlags :: [FLAG] -> ([ColourRGB] -> [ColourRGB])
getBuilderFromFlags = 
    switchFlags 
        [METHOD_KMEANS] 
        (\ f -> case f of 
            METHOD_KMEANS -> buildTerminalColoursKMeans)

getFormatterFromFlags :: String -> [FLAG] -> ([ColourRGB] -> String)
getFormatterFromFlags imgpath = 
    switchFlags 
        [FORMAT_XGCM, FORMAT_XRESOURCES] 
        (\ f -> case f of 
            FORMAT_XGCM -> showColoursXGCM imgpath
            FORMAT_XRESOURCES -> showColoursXResources)


-- split path on image decoding error
either' switch fail succeed = either fail succeed switch

imgSuccess :: [FLAG] -> String  -> DynamicImage -> IO()
imgSuccess flags imgpath img = do
    -- putStrLn $ getColourSpaceName img
    let buildColours = getBuilderFromFlags flags
        showColours  = getFormatterFromFlags imgpath flags

        imgPixelsRGB = processDynamicImage img
        imgPixelsLAB = map convertToLAB imgPixelsRGB
        coloursLAB = buildColours imgPixelsLAB
        coloursRGB = map convertToRGB coloursLAB
    putStrLn $ showColours $ coloursRGB

exitWithError err = do
    putStrLn err
    exitWith (ExitFailure 1)

imgFailure :: String -> IO()
imgFailure msg = exitWithError $ "Error decoding image:\n" ++ msg

printHelpText :: IO()
printHelpText = do
    progName <- getProgName
    putStrLn $ helpText progName
    exitWith ExitSuccess

main :: IO()
main = do
    argv <- getArgs
    either' (getOpt argv) exitWithError (\opts ->
        do
        let (flags, imagePaths) = opts

        if HELP `elem` flags || length imagePaths == 0
            then printHelpText
            else return ()

        -- throw error if multiple images passed over stdin
        if length imagePaths > 1
            then exitWithError "can only process a single image"
            else return ()

        -- read from stdin if argument is "-" or if no arguments
        -- otherwise, read from the path specfied by the first argument
        im <- if (imagePaths !! 0) == "-"
            then do imStream <- hGetContents stdin
                    return (decodeImage imStream)
            else readImage (imagePaths !! 0)

        let impath = if (imagePaths !! 0) == "-"
                then ""
                else imagePaths !! 0

        either' im imgFailure (imgSuccess flags impath) )
