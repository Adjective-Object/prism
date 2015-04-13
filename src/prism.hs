module Main where

-- Simple stuff for IO
import System.IO (stdin)
import Data.ByteString (hGetContents)
import Data.String.Utils(split, join)
import System.Exit (exitWith, ExitCode(..))

-- codec imports
import Codec.Picture.Types(pixelFold, convertImage, ColorSpaceConvertible)
import Codec.Picture (decodeImage,
                        readImage,
                        DynamicImage (..))

-- My Modules
import TerminalColours (buildTerminalColours)
import ImageAbstractions (processDynamicImage, convertToRGB, convertToLAB)
import IOHelpers (showColoursXGCM, showColoursXResources)




-- Helper functions for input and output
exitWithError :: String -> IO a
exitWithError err = do putStrLn err
                       exitWith (ExitFailure 1)

imgSuccess :: DynamicImage -> IO()
imgSuccess img = do
            -- putStrLn $ getColourSpaceName img
            let imgPixelsRGB = processDynamicImage img
                imgPixelsLAB = map convertToLAB imgPixelsRGB
                coloursLAB = buildTerminalColours imgPixelsLAB
                coloursRGB = map convertToRGB coloursLAB
            putStrLn $ showColoursXGCM $ coloursRGB

imgFailure :: String -> IO()
imgFailure msg = do
            exitWithError $ "Error decoding image:\n" ++ msg

main :: IO()
main = do
    -- im <- readImage "images/dock_tiny.jpg"
    imStream <- hGetContents stdin
    let im = decodeImage imStream
    either imgFailure imgSuccess im
