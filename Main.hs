module Main where
import System.Exit (exitWith, ExitCode(..))
import Data.KMeans (kmeans, kmeansGen)
import Codec.Picture (decodeImage, pixelMap,
                        Image,
                        DynamicImage(ImageRGB8),
                        PixelRGB8(..) )
import System.IO (stdin)
import Data.ByteString (hGetContents)

exitWithError :: String -> IO a
exitWithError err = do putStrLn err
                       exitWith (ExitFailure 1)

rgbToDoubleList :: PixelRGB8 -> [Double]
rgbToDoubleList (PixelRGB8 r g b) =
    let words = [r, g, b]
        in map fromIntegral words

convertImageToDoubleList :: DynamicImage -> [[Double]]
convertImageToDoubleList img = 
    case img of
        ImageRGB8 i -> pixelMap rgbToDoubleList i
        _ -> [[0.0]]

main :: IO()
main = do
    putStrLn "reading Image"
    imstream <- hGetContents stdin
    putStrLn "reading complete"
    either failure success (decodeImage imstream)
        where 
            success img = 
                do  let doubleImg = convertImageToDoubleList img
                    exitWithError "successPath not implemented"
            failure msg =
                do  exitWithError $ "Error decoding image:\n" ++ msg

