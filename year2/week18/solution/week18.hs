{-# LANGUAGE TemplateHaskell #-}

import Data.List.Split (chunksOf)
import Data.Lens.Light (makeLenses, modL)

type RGB = (Int, Int, Int)
type ImageData = [RGB]
data Image = Image {
    _imageType  :: String,
    _width      :: Int,
    _height     :: Int,
    _imageData  :: ImageData
}

makeLenses [''Image]

parseRGB :: [Int] -> RGB
parseRGB [r, g, b] = (r, g, b)

-- | Assumes that the meta data has already been striped
parseImageData :: String -> ImageData
parseImageData = map parseRGB . chunksOf 3 . map read . words

-- | Parses entire image with meta data
parseImage :: String -> Image
parseImage s = Image imageType w h d
    where
        x         = lines s
        imageType = head x
        [w, h]    = (map read . words) (x !! 1)
        d         = parseImageData $ unlines $ drop 3 x

-- | Given an image, transform the data to grayscale and
--   return a new image
grayScale :: Image -> Image
grayScale = imageData `modL` (map rgbToGrayScale)

-- | Convert a color pixel to grayscale
rgbToGrayScale :: RGB -> RGB
rgbToGrayScale (r, g, b) = (avg, avg, avg)
    where avg = (r + g + b) `div` 3

showRGB :: RGB -> String
showRGB (r, g, b) = (show r) ++ " " ++ (show g) ++ " " ++ (show b)

-- | Convert image to PPM format
formatPPM :: Image -> String
formatPPM (Image imageType width height d) = unlines $ meta ++ (map showRGB d)
    where meta = [imageType, show width, show height, "255"]

main = interact $ formatPPM . grayScale . parseImage
