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
charAt   [  B,   B,   B,
          x21, x22, x23,
          x31, x32, x33]
  | oneOrNone (== B) l2 && oneOrNone (== B) l3 = "--"
  where l2 = [x21, x22, x23]
        l3 = [x31, x32, x33]
charAt   [x11, x12, x13,
            B,   B,   B,
          x31, x32, x33]
  | oneOrNone (== B) l1 && oneOrNone (== B) l3 = "--"
  where l1 = [x11, x12, x13]
        l3 = [x31, x32, x33]
charAt   [x11, x12, x13,
          x21, x22, x23,
            B,   B,   B]
  | oneOrNone (== B) l1 && oneOrNone (== B) l2 = "__"
  where l1 = [x11, x12, x13]
        l2 = [x21, x22, x23]
charAt   [x11, x12, x13,
            B,   B,   B,
            B,   B,   B]
  | oneOrNone (== B) l1 = "~~"
  where l1 = [x11, x12, x13]
charAt   [  B,   B,   B,
            B,   B,   B,
          x31, x32, x33]
  | oneOrNone (== B) l3 = "~~"
  where l3 = [x31, x32, x33]

-- vlines
charAt   [B, x12, x13,
          B, x22, x23,
          B, x32, x33]
  | oneOrNone (== B) c2 && oneOrNone (== B) c3 = "| "
  where c2 = [x12, x22, x32]
        c3 = [x13, x23, x33]
charAt   [x11, B, x13,
          x21, B, x23,
          x31, B, x33]
  | oneOrNone (== B) c1 && oneOrNone (== B) c3 = "| "
  where c1 = [x11, x21, x31]
        c3 = [x13, x23, x33]
charAt   [x11, x12, B,
          x21, x22, B,
          x31, x32, B]
  | oneOrNone (== B) c1 && oneOrNone (== B) c2 = " |"
  where c1 = [x11, x21, x31]
        c2 = [x12, x22, x32]
charAt   [B, B, x13,
          B, B, x23,
          B, B, x33]
  | oneOrNone (== B) c3 = "||"
  where c3 = [x13, x23, x33]
charAt   [x11, B, B,
          x21, B, B,
          x31, B, B]
  | oneOrNone (== B) c1 = "||"
  where c1 = [x11, x21, x31]

charAt x = "  "
oneOrNone :: (a -> Bool) -> [a] -> Bool
oneOrNone f x = length (filter f x) <= 1

presedence = ['\\', '/', '_', '-', '|', '#', ' ']

boxToChar :: [BW] -> Char
boxToChar box = case (c1, c2) of
  ('-', '-') -> '_'
  _          -> if c1idx <= c1idx then c1 else c2
  where
    c1 = charAt' (take 4 box)
    c2 = charAt' (drop 4 box)
    c1idx = fromMaybe 99 (elemIndex c1 presedence)
    c2idx = fromMaybe 99 (elemIndex c2 presedence)

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
