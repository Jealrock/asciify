{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where
import System.Environment( getArgs )
import Codec.Picture
import Codec.Picture.Types

convertRGBF :: DynamicImage -> Image PixelRGBF
convertRGBF = promoteImage . convertRGB8

convertF :: DynamicImage -> Image PixelF
convertF = extractLumaPlane . convertRGBF

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

pixelsIn :: (Int, Int) -> (Int, Int) -> Image PixelF -> [PixelF]
pixelsIn (x, y) (x', y') img = concatMap (\ yi -> map (\ x -> pixelAt img x yi) [x..x']) [y..y']

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

    expose px = min ((px - black_level) * gain) 1.0

data BW = B | W deriving (Show, Eq) -- Black/White

preAsciify :: Image PixelF -> [[[BW]]]
preAsciify img = map (\ y -> map (`bwify` y) [0..w-1]) [0..h-1]
  where
    xdim = 3
    ydim = 3
    w = quot (imageWidth img) xdim
    h = quot (imageHeight img) ydim

    bwify x y = map (\x -> if x > 0.5 then W else B) $
                pixelsIn (x*xdim, y*ydim) (x*xdim+xdim-1, y*ydim+ydim-1) img


oneOrNone :: (a -> Bool) -> [a] -> Bool
oneOrNone f x = length (filter f x) <= 1

charAt :: [BW] -> String
-- diagonals
charAt x@[B, x12, x13,
          x21, B, x23,
          x31, x32, B]
  | oneOrNone (== B) rut && oneOrNone (== B) lbt = " \\"
  where rut = [x12, x13, x23] -- right upper triangle
        lbt = [x21, x31, x32] -- left bottom triangle
charAt   [B, B, W,
          B, B, B,
          W, B, B] = "\\\\"
charAt   [B, B, W,
          W, B, B,
          W, W, B] = " \\"
charAt   [B, W, W,
          B, B, W,
          W, B, B] = "\\ "
charAt x@[x11, x12, B,
          x21, B, x23,
          B, x32, x33]
  | oneOrNone (== B) lut && oneOrNone (== B) rbt = " /"
  where lut = [x11, x12, x21] -- left upper triangle
        rbt = [x23, x32, x33] -- right bottom triangle
charAt   [W, W, B,
          W, B, B,
          B, B, W] = " /"
charAt   [W, B, B,
          B, B, W,
          B, W, W] = "/ "
charAt   [W, B, B,
          B, B, B,
          B, B, W] = "//"
-- hlines

-- vlines
charAt x = "  "

asciify :: Image PixelF -> [String]
asciify img = map (concatMap charAt) (preAsciify img)

edgeConv = [[  0.0, -1.0,  0.0],
            [ -1.0,  4.0, -1.0],
            [  0.0, -1.0,  0.0]]

asciifySize = 3
desiredSize = 40
maxPoolSize = 4

main :: IO ()
main = do
  commandArguments <- getArgs
  case commandArguments of
    [] -> putStrLn "Not enough arguments"
    (filename : _) -> do
      dynImg <- readImage filename
      case dynImg of
        Left err -> putStrLn err
        Right img -> do
          let greyscale = convertF img
          let w = imageWidth greyscale
          -- I want 60x20
          -- How much pools should be done?
          let poolBy = quot w (desiredSize * asciifySize)
          let pools = quot poolBy maxPoolSize
          let lastPoolSize = maxPoolSize - rem poolBy maxPoolSize

          let processList =
                [pooling (lastPoolSize, lastPoolSize)]
                ++ replicate pools (pooling (maxPoolSize, maxPoolSize))
                ++ [exposure (0.1, 10.0)]

          let processed = foldr (\f acc -> f acc) greyscale processList

          saveJpgImage 80 (filename ++ "_processed.jpg") (ImageYF processed)
          mapM_ putStrLn (asciify processed)
