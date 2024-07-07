module Asciify.Picture
  (module Asciify.Picture,
   module Codec.Picture,
   module Codec.Picture.Types) where

import Codec.Picture
import Codec.Picture.Types

padImg :: (Int, Int) -> Image PixelF -> Image PixelF
padImg (xpad, ypad) img@(Image{imageWidth = ow, imageHeight = oh}) =
  generateImage pixel (ow + xpad * 2) (oh + ypad * 2)
  where
    yfrom y | y < ypad  = 0
            | y >= oh   = oh - 1
            | otherwise = y
    xfrom x | x < xpad  = 0
            | x >= ow   = ow - 1
            | otherwise = x
    pixel x y = pixelAt img (xfrom x) (yfrom y)

pooling :: (Int, Int) -> Image PixelF -> Image PixelF
pooling (xdim, ydim) img = generateImage pool w h
  where
    w = quot (imageWidth img) xdim
    h = quot (imageHeight img) ydim
    pool x y = minimum (pixelsIn (x*xdim, y*ydim) (x*xdim+xdim-1, y*ydim+ydim-1) img)

convolution :: [[Float]] -> Image PixelF -> Image PixelF
convolution matrix img = pixelMapXY convolute img
  where
    xpad = quot (length matrix) 2
    ypad = quot (length (head matrix)) 2
    paddedImg = padImg (xpad, ypad) img
    pixels x y = pixelsIn (x, y) (x+xpad*2, y+ypad*2) paddedImg

    convolute x y px = sum $ zipWith (*) (concat matrix) (pixels x y)

exposure :: (Float, Float) -> Image PixelF -> Image PixelF
exposure (black_level, exposure) = pixelMap expose
  where
    white = 2 ** (-exposure)
    diff = max (white - black_level) 0.000001
    gain = 1.0 / diff;

    expose px = (px - black_level) * gain

inverse :: Image PixelF -> Image PixelF
inverse = pixelMap (1.0 -)

threshold :: Float -> Image PixelF -> Image PixelF
threshold x = pixelMap (\ p -> if p > x then 1.0 else 0.0)

convertRGBF :: DynamicImage -> Image PixelRGBF
convertRGBF = promoteImage . convertRGB8

convertF :: DynamicImage -> Image PixelF
convertF = extractLumaPlane . convertRGBF

pixelsIn :: (Int, Int) -> (Int, Int) -> Image PixelF -> [PixelF]
pixelsIn (x, y) (x', y') img = concatMap (\ yi -> map (\ x -> pixelAt img x yi) [x..x']) [y..y']

-- debugging
fToRGBF :: Image PixelF -> Image PixelRGBF
fToRGBF = promoteImage

boxify :: (Int, Int) -> Image PixelRGBF -> Image PixelRGBF
boxify (bw, bh) img@(Image{imageWidth = ow, imageHeight = oh}) = generateImage pixel w h
  where
    boxesw = quot ow bw
    boxesh = quot oh bh
    w = ow + boxesw + 1
    h = oh + boxesh + 1
    emptyPixel = PixelRGBF 0.5 0.5 0.5

    pixel x y | x == 0 || y == 0 = emptyPixel
              | x `mod` (bw + 1) == 0 || y `mod` (bh + 1) == 0 = emptyPixel
              | otherwise = pixelAt img (x - (x `div` (bw + 1) + 1))
                                        (y - (y `div` (bh + 1) + 1))
