module Main where
import System.Exit (exitWith, ExitCode(..))
import Data.List (transpose)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as DV
import Data.Functor.Identity(Identity, runIdentity)
import Math.KMeans

import System.Random
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

image_mult_constant = 10

-- Folds an image into a list of Ints so it can be consumed by kheap
collapseToVectors ::
    [V.Vector Int] -> Int -> Int -> PixelRGB8 -> [V.Vector Int]
collapseToVectors lst x y (PixelRGB8 r g b) =
    let words = r <:> ( g <:> ( b <:> V.empty))
        in lst ++ [V.map (\ word -> fromIntegral word :: Int) words]

convertImageToVectors :: DynamicImage -> [V.Vector Int]
convertImageToVectors img =
    let c img = pixelFold collapseToVectors []  img
        ci = convertImage
    in case img of
        ImageRGB8   i   -> c i
        ImageYCbCr8 i   -> c (ci i :: Image PixelRGB8)
        _ -> [V.singleton 0]

-------------------------------------------
-- Calling kmeans and parsing the output --
-------------------------------------------

-- | Used to do the initial partition with given initial values
knownPartition :: [Int] -> Int -> [a] -> Identity (Clusters a)
knownPartition indecies numPartitions objs = 
    let sampledPts = map (\i -> objs !! i) indecies
        initialGroups = map (\a->Cluster [a]) sampledPts
    in return (DV.fromList initialGroups)

-- | takes a cluster of vectors of Ints and gets the average value
averageCluster :: Cluster(V.Vector Int) -> V.Vector Double
averageCluster cluster = let 
        vectors :: [V.Vector Int]
        vectors = elements cluster
        len = fromIntegral $ length vectors :: Double
        sumTuple2 = (\ (a, b) -> a + b )
        sumOfVectors :: V.Vector Int
        sumOfVectors = foldl1 (\ v1 v2 -> V.map sumTuple2 (V.zip v1 v2)) vectors
    in V.map (\a -> (fromIntegral a :: Double)/len) sumOfVectors

-- | Builds 'terminal ready' colours from a vector of Ints
buildTerminalColours :: [V.Vector Double] -> [V.Vector Double]
buildTerminalColours pixels = pixels

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

rgbToHexCode :: RealFrac a =>[a] -> String
rgbToHexCode channels = foldl (++) "#" hexes
    where   intRGB = (map floor channels)
            hexesShowS = map showHex intRGB
            shortHexes = map (\ showS -> showS "") hexesShowS
            hexes = map (\ x -> if length x < 2
                                    then "0" ++ x
                                    else x) shortHexes

runKMeans :: [Int] -> [V.Vector Int] -> Clusters (V.Vector Int)
runKMeans randIndecies imgPixels = 
    let randPartition = knownPartition randIndecies
        toFloatVector = (\ v -> V.map (\ i -> fromIntegral i :: Double) v)
        --kmeans_id :: Identity (Clusters (V.Vector Int))
        --kmeans_id = kmeansWith randPartition toFloatVector l1dist 100 imgPixels
    --in runIdentity kmeans_id
    in kmeans toFloatVector l1dist 100 imgPixels

uniqueSample :: StdGen -> Int -> Int -> [Int]
uniqueSample stdGen maxVal n =
    let rs = map (\ a -> a `mod` maxVal) $ randoms stdGen
        unique_rs = map (\ (_, v) -> v) 
            $ filter (\ (i, v) -> not $ v `elem` take i rs) (zip [0..] rs)
    in take n unique_rs


_pretty :: Show a => Int -> [a] -> String
_pretty _ [] = ""
_pretty ind (x:xs) = 
    let thispretty = case x of
            y -> show y
    in replicate ind '\t' ++ thispretty ++ "\n" ++ _pretty ind xs 
pretty :: Show a => [a] -> String
pretty = _pretty 0

main :: IO()
main = do
    putStrLn "reading ImageRGB8e"
    imstream <- hGetContents stdin
    either failure success (decodeImage imstream)

success:: DynamicImage -> IO()
success img = do
    putStrLn $ "image colour space: " ++ getColourSpaceName img

    -- put the image into image vectors, eagerly
    let imgPixels = id $! convertImageToVectors img

    -- generate the initial sample indicies
    stdGen <- getStdGen
    let randIndecies = uniqueSample stdGen (length imgPixels) 0
        
    putStrLn $ (++)
        "Initial Indecies: "
        $ show randIndecies

    -- perform kmeans, 
    let kmeans_out = runKMeans randIndecies imgPixels

    putStrLn $ (++)
        "length kmeans_out: "
        $ show $ DV.length kmeans_out

    -- average each colour, and generate the terminal colours
    -- from them
    let cluster_averages = DV.toList $ DV.map averageCluster kmeans_out
        colours = buildTerminalColours cluster_averages
        coloursList = map V.toList colours

    putStrLn $ show cluster_averages
    putStrLn $ show coloursList
    putStrLn $ show $ map rgbToHexCode coloursList

failure :: String -> IO()
failure msg = do exitWithError $ "Error decoding image:\n" ++ msg

