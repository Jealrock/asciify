{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where
import System.Environment( getArgs )
import Codec.Picture
import Codec.Picture.Types

{-
  We want an output to be 70x20
  or maybe having 2 chars per convolution so that it will be more sizable

  Convolution must be small, like <4 probably?
  Find a set of convolution operations for a given image width and height

  Would love to use cutting code for convolution and asciifying from pigworker/CS410-17
  Is there a similar data structure in stdlib?
  Maybe it's something used in xmonad?
  xmonad seems to have similar structure, but it has a lot of overhead having to respond
  to user commands, which we don't have to do yet
-}

 --   _ _ _ _ _
 -- | 4       4
 -- |   4   4
 -- |     4
 -- |   4   4
 -- | 4       4

instance ColorSpaceConvertible PixelRGB8 Pixel8 where
  {-# INLINE convertPixel #-}
  -- TODO: use better algorithm (see color lecture)
  -- convertPixel (PixelRGB8 r g b) = fromIntegral (floor (0.33 * (fromIntegral ((fromIntegral r) + (fromIntegral g) + (fromIntegral b)))))
  convertPixel (PixelRGB8 r g b) = max r (max g b)

convertP8 :: DynamicImage -> Image Pixel8
convertP8 = convertImage . convertRGB8

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

data BW = B | W -- Black/White

preAsciify :: Image PixelF -> [[[BW]]]
preAsciify img = map (\ y -> map (`bwify` y) [0..w-1]) [0..h-1]
  where
    xdim = 3
    ydim = 3
    w = quot (imageWidth img) xdim
    h = quot (imageHeight img) ydim

    bwify x y = map (\x -> if x > 0.5 then W else B) $
                pixelsIn (x*xdim, y*ydim) (x*xdim+xdim-1, y*ydim+ydim-1) img


asciify :: Image PixelF -> [String]
asciify img = map (map charAt) (preAsciify img)
  where
    charAt :: [BW] -> Char
    charAt [W, W, W,
            W, W, W,
            W, W, W] = ' '
    charAt x = '#'

edgeConv = [[  0.0, -1.0,  0.0],
            [ -1.0,  4.0, -1.0],
            [  0.0, -1.0,  0.0]]

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
          let processed = (pooling (4, 4) . exposure (0.1, 10.0)) greyscale

          saveJpgImage 80 (filename ++ "_processed.jpg") (ImageYF processed)
          mapM_ putStrLn (asciify processed)
