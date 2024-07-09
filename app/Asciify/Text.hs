module Asciify.Text where
import Asciify.Picture
import Data.Maybe
import Data.List

data BW = B | W deriving (Show, Eq) -- Black/White

preAsciify :: Image PixelF -> [[[BW]]]
preAsciify img = map (\ y -> map (`bwify` y) [0..w-1]) [0..h-1]
  where
    xdim = 2
    ydim = 4
    w = quot (imageWidth img) xdim
    h = quot (imageHeight img) ydim

    bwify x y = map (\x -> if x > 0.0 then W else B) $
                pixelsIn (x*xdim, y*ydim) (x*xdim+xdim-1, y*ydim+ydim-1) img

boxToChar :: [BW] -> Char
boxToChar box = case (c1, c2) of
  ('-', '-') -> '_'
  ('#', '#') -> '#'
  ('#', _)   -> '-'
  (_ , '#')  -> '_'
  (_, _)     -> if c1idx <= c2idx then c1 else c2
  where
    c1 = charAt (take 4 box)
    c2 = charAt (drop 4 box)
    c1idx = fromMaybe 99 (elemIndex c1 presedence)
    c2idx = fromMaybe 99 (elemIndex c2 presedence)

presedence = ['\\', '/', '_', '-', '|', '#', ' ']

charAt :: [BW] -> Char
charAt [W, W, W, W] = ' '
charAt [B, W, W, W] = ' '
charAt [W, B, W, W] = ' '
charAt [W, W, B, W] = ' '
charAt [W, W, W, B] = ' '
charAt [B, B, W, W] = '-'
charAt [B, W, B, W] = '|'
charAt [B, W, W, B] = '\\'
charAt [W, B, B, W] = '/'
charAt [W, B, W, B] = '|'
charAt [W, W, B, B] = '_'
charAt [B, B, B, W] = '/'
charAt [B, W, B, B] = '\\'
charAt [B, B, W, B] = '\\'
charAt [W, B, B, B] = '/'
charAt [B, B, B, B] = '#'

asciify :: Image PixelF -> [String]
asciify img = map (map boxToChar) (preAsciify img)
