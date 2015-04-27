module ImageAbstractions where

import Data.Prizm.Color.CIE.LAB(fromRGB, toRGB)
import Data.Prizm.Types(CIELAB(CIELAB), RGB(RGB))

import Debug.Trace

import Codec.Picture.Types(
    convertPixel,
    pixelAt,
    Pixel,
    ColorSpaceConvertible)
import Codec.Picture (
    Image(imageWidth, imageHeight),
    DynamicImage (..),
    PixelRGB8 (..))

type ColourRGB = [Double]
type ColourLAB = [Double]

traceVal x = trace (show x) x
traceSeq x = seq $ trace (show x) x

--for sampling pixels from an image
pixelToDoubleList :: PixelRGB8 -> ColourRGB
pixelToDoubleList (PixelRGB8 r g b) =
    let words = [r, g, b]
    in  map (\ word -> fromIntegral word :: Double) words

gridSample :: Image a -> (Int,Int) -> [(Int, Int)]
gridSample img (gridx, gridy) =
    let width = (imageWidth img) `quot` gridx
        height = (imageHeight img) `quot` gridy
        --t = trace $ show (gridx, gridy)
        --tt = t $ trace $ show (width, height)
        gencoords = (\ dist ->
            (zip [0..(min dist width)] (repeat (min dist height))) ++
            (zip (repeat (min dist width)) [0..(min dist height)]) )
    in  map (\ (x, y) -> (gridx * x, gridy * y)) $
        foldl (++) [(0,0)] (map gencoords [1..((max width height) - 1)])

samplePixelsFromImage :: (Pixel a) => Image a -> [(Int,Int)] -> [a]
samplePixelsFromImage img coordSet = 
    map (\ (x, y) -> pixelAt img x y) coordSet

sampleFromImage :: (ColorSpaceConvertible a PixelRGB8) => 
                        Image a -> (Int,Int) -> [ColourRGB]
sampleFromImage img gridsize = 
    let coords = gridSample img gridsize
        pixels = samplePixelsFromImage img coords
        rgb8Pixels :: [PixelRGB8]
        rgb8Pixels = map convertPixel pixels
    in  map pixelToDoubleList rgb8Pixels

sampleFromDynamicImage :: DynamicImage -> [ColourRGB]
sampleFromDynamicImage img = case img of
    (ImageRGB8     i) -> sample i
    (ImageYCbCr8   i) -> sample i
    where sample img = sampleFromImage img ( ((imageWidth  img) `quot` 20), 
                                             ((imageHeight img) `quot` 20))
        
            

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
