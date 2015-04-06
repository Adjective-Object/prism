module Main where
import System.Exit (exitWith, ExitCode(..))
import Data.List (transpose)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as DV
import Math.KMeans
import Numeric (showHex)
import Codec.Picture (decodeImage,
                        Image,
                        DynamicImage (..),
                        PixelRGB8 (..) )
import Codec.Picture.Types(pixelFold, convertImage, ColorSpaceConvertible)
import System.IO (stdin)
import Data.ByteString (hGetContents)

-- quick helper because V.cons takes too long to type out
(<:>) = V.cons

-- Folds an image into a list of doubles so it can be consumed by kheap
collapseToVectors ::
    [V.Vector Double] -> Int -> Int -> PixelRGB8 -> [V.Vector Double]
collapseToVectors lst x y (PixelRGB8 r g b) =
    let words = r <:> ( g <:> ( b <:> V.empty))
        in lst ++ [V.map (\ word -> fromIntegral word :: Double) words]

convertImageToVectors :: DynamicImage -> [V.Vector Double]
convertImageToVectors img =
    let c img = pixelFold collapseToVectors []  img
        ci = convertImage
    in case img of
        ImageRGB8   i   -> c i
        ImageYCbCr8 i   -> c (ci i :: Image PixelRGB8)
        _ -> [V.singleton 0.0]

-------------------------------------------
-- Calling kmeans and parsing the output --
-------------------------------------------

-- | takes a cluster of vectors of doubles and gets the average value
averageCluster :: Cluster(V.Vector Double) -> V.Vector Double
averageCluster cluster =
    let vectors :: [V.Vector Double]
        vectors = elements cluster
        len = fromIntegral $ length vectors :: Double
        sumTuple2 = (\ (a, b) -> a + b )
        sumVectors = foldl1 (\ v1 v2 -> V.map sumTuple2 (V.zip v1 v2)) vectors
    in V.map (\a -> a/len) sumVectors

-- | Builds 'terminal ready' colours from a vector of doubles
buildTerminalColours :: [V.Vector Double] -> [V.Vector Double]
buildTerminalColours pixels =
    let maxTries = 100
        clusters = kmeans id euclidSq 6 pixels
        colourChannels = DV.map averageCluster clusters
    in DV.toList colourChannels

-------------------------------------------
-- Helper functions for input and output --
-------------------------------------------

-- | gets the string name of a colour space, Used in reporting image format
-- from Codec.Picture.DynamicImage
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

main :: IO()
main = do
    putStrLn "reading ImageRGB8e"
    imstream <- hGetContents stdin
    either failure success (decodeImage imstream)
    where   success img =
                do  putStrLn $ "image colour space: " ++ getColourSpaceName img

                    let imgPixels = convertImageToVectors img
                        g = kmeans id euclidSq 6 imgPixels
                        colours = buildTerminalColours imgPixels
                        coloursList = map V.toList colours

                    putStrLn $ (++) "lengths: "
                                $ show
                                $ DV.map (\a -> length (elements a)) g

                    putStrLn $ show coloursList
                    putStrLn $ show $ map rgbToHexCode coloursList
            failure msg =
                do  exitWithError $ "Error decoding image:\n" ++ msg

