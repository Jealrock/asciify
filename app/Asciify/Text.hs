module Asciify.Text where
import Asciify.Picture
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)

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
  _          -> if c1idx <= c2idx then c1 else c2
  where
    c1 = charAt' (take 4 box)
    c2 = charAt' (drop 4 box)
    c1idx = fromMaybe 99 (elemIndex c1 presedence)
    c2idx = fromMaybe 99 (elemIndex c2 presedence)

presedence = ['\\', '/', '_', '-', '|', '#', ' ']

boxToChar' :: [BW] -> Char
boxToChar' box = case (c1, c2, c3) of
  ('#', '#', '#') -> '#'
  ('#', _, _)   -> '\''
  (_, '#', _)   -> '-'
  (_ , _, '#')  -> '_'
  _          -> if min c1idx <= c2idx then c1 else c2
  where
    twoSame
    c1 = charAt' (take 4 box)
    c2 = charAt' ((drop 4 . take 6) box)
    c3 = charAt' (drop 4 box)
    clist = [c1 : c2 : c3]
    idx = \ x -> fromMaybe 99 (elemIndex x presedence)
    ci = zip (map idx clist) clist

charAt' :: [BW] -> Char
charAt' x@[W, W, W, W] = ' '
charAt' x@[B, W, W, W] = ' '
charAt' x@[W, B, W, W] = ' '
charAt' x@[W, W, B, W] = ' '
charAt' x@[W, W, W, B] = ' '
charAt' x@[B, B, W, W] = '-'
charAt' x@[B, W, B, W] = '|'
charAt' x@[B, W, W, B] = '\\'
charAt' x@[W, B, B, W] = '/'
charAt' x@[W, B, W, B] = '|'
charAt' x@[W, W, B, B] = '_'
charAt' x@[B, B, B, W] = '/'
charAt' x@[B, W, B, B] = '\\'
charAt' x@[B, B, W, B] = '\\'
charAt' x@[W, B, B, B] = '/'
charAt' x@[B, B, B, B] = '#'

asciify :: Image PixelF -> [String]
asciify img = map (map boxToChar) (preAsciify img)
