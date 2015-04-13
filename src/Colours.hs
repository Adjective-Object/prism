module ImageAbstractions where
import Data.Prizm.Color.CIE.LAB(fromRGB, toRGB)
import Data.Prizm.Types(CIELAB(CIELAB), RGB(RGB))
import Codec.Picture.Types(
    pixelFold, 
    convertImage, 
    ColorSpaceConvertible)
import Codec.Picture ( 
    Image,
    DynamicImage (..),
    PixelRGB8 (..))

type ColourRGB = [Double]
type ColourLAB = [Double]


-- Folds an image into a list of doubles so it can be consumed by kheap
collapseToDoubleList :: [ColourRGB] -> Int -> Int -> PixelRGB8 -> [ColourRGB]
collapseToDoubleList lst x y (PixelRGB8 r g b) =
    let words = [r, g, b]
        in lst ++ [map (\ word -> fromIntegral word :: Double) words]

processDynamicImage :: DynamicImage -> [ColourRGB]
processDynamicImage img =
    let c img = pixelFold collapseToDoubleList []  img
        ci = convertImage
    in case img of
        ImageRGB8   i   -> c i
        ImageYCbCr8 i   -> c (ci i :: Image PixelRGB8)
        _ -> [[0.0]]


-- Colour Space Conversions
toDouble :: Integral a => a -> Double
toDouble i = fromIntegral i :: Double

convertToLAB :: ColourRGB -> ColourLAB
convertToLAB [r, g, b] = let
        rgb :: RGB Integer
        rgb = (RGB (floor r) (floor g) (floor b))
        lab :: CIELAB Double
        lab = fromRGB rgb
        CIELAB _l _a _b = lab
    in [_l, _a, _b]

convertToRGB :: ColourLAB -> ColourRGB
convertToRGB [l, a, b] = let 
        lab  :: CIELAB Double
        lab = (CIELAB l a b)
        rgb  :: RGB Integer     
        rgb = toRGB lab
        RGB _r _g _b = rgb
    in map toDouble [_r, _g, _b]
