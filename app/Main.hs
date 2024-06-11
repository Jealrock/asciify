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

padImg :: (Int, Int) -> Image Pixel8 -> Image Pixel8
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

pixelsIn :: (Int, Int) -> (Int, Int) -> Image Pixel8 -> [Pixel8]
pixelsIn (x, y) (x', y') img = concatMap (\ yi -> map (\ x -> pixelAt img x yi) [x..x']) [y..y']

pooling :: (Int, Int) -> Image Pixel8 -> Image Pixel8
pooling (xdim, ydim) img = generateImage pool w h
  where
    w = quot (imageWidth img) xdim - 1
    h = quot (imageHeight img) ydim - 1
    pool x y = minimum (pixelsIn (x*xdim, y*ydim) (x*xdim+xdim-1, y*ydim+ydim-1) img)

convolution :: [[Int]] -> Image Pixel8 -> Image Pixel8
convolution matrix img = pixelMapXY convolute img
  where
    xdim = length matrix
    ydim = length (head matrix)
    paddedImg = padImg (quot xdim 2, quot ydim 2) img
    pixels x y = pixelsIn (x*xdim, y*ydim) (x*xdim+xdim-1, y*ydim+ydim-1) paddedImg

    convolute x y px = fromIntegral $ sum $ zipWith (\ x y -> x * fromIntegral y) (concat matrix) (pixels x y)

charFor :: Pixel8 -> Char
charFor px | px < 51   = '#'
           | px < 102  = '*'
           | px < 153  = ':'
           | px < 204  = '.'
           | otherwise = ' '

asciify :: Image Pixel8 -> [String]
asciify img@(Image{imageWidth = w, imageHeight = h}) =
  map (\ y -> map (`charAt` y) [0..w-1]) [0..h-1]
  where
    -- widen = concatMap (replicate 2)
    charAt x y = charFor $ pixelAt img x y

edgeConv = [[ 0, -1,  0],
            [-1,  4, -1],
            [ 0, -1,  0]]

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
          let greyscale = convertP8 img
          -- savePngImage (filename ++ "_greyscale.png") (ImageY8 greyscale)
          let processed = (pooling (4, 4) . convolution edgeConv . pooling (2, 2)) greyscale
          saveJpgImage 80 (filename ++ "_transformed.jpg") (ImageY8 processed)
          mapM_ putStrLn (asciify processed)
