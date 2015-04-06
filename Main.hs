module Main where
import System.Exit (exitWith, ExitCode(..))
import Data.List (transpose)
import Data.KMeans (kmeans)
import Codec.Picture (decodeImage,
                        Image,
                        DynamicImage (..),
                        PixelRGB8 (..) )
import Codec.Picture.Types(pixelFold)
import System.IO (stdin)
import Data.ByteString (hGetContents)


-- Folds an image into a list of doubles so it can be consumed by kheap
collapseToDoubleList :: [[Double]] -> Int -> Int -> PixelRGB8 -> [[Double]]
collapseToDoubleList lst x y (PixelRGB8 r g b) =
    let words = [r, g, b]
        in lst ++ [map (\ word -> fromIntegral word :: Double) words]

convertImageToDoubleList :: DynamicImage -> [[Double]]
convertImageToDoubleList img = 
    case img of
        ImageRGB8 i -> pixelFold collapseToDoubleList [] i
        _ -> [[0.0]]


-- Build a set of terminal colours using kmeans from Data.KMeans
-- returning colours in RGB format
buildTerminalColours :: [[Double]] -> [[Double]]
buildTerminalColours pixels = 
    let maxTries = 100
        blocks = kmeans 6 pixels
        colourChannels = map transpose blocks
    in map (\ channels -> map avg channels ) colourChannels

avg :: [Double] -> Double
avg a = (sum a) / (fromIntegral (length a) :: Double)


-- Functions for input and output
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

main :: IO()
main = do
    putStrLn "reading Image"
    imstream <- hGetContents stdin
    putStrLn "reading complete, passing to kmeans"
    either failure success (decodeImage imstream)
    where   success img = 
                do  putStrLn $ "image colour space: " ++ getColourSpaceName img
                    let imgPixels = convertImageToDoubleList img
                        colours = buildTerminalColours imgPixels
                    putStrLn $ show imgPixels
            failure msg =
                do  exitWithError $ "Error decoding image:\n" ++ msg

