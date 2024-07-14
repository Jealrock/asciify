{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}

module Main where
import System.Environment
import System.Console.CmdArgs
import System.Exit
import Control.Monad (when, foldM)
import Asciify.Picture
import Asciify.Text

asciifySize = 4
maxPoolSize = 4

pools :: Int -> Int -> [Int]
pools maxPoolSize poolBy = case quotRem poolBy maxPoolSize of
  (q, r) | q == 0 && r == 0 -> []
         | q == 0 -> [maxPoolSize]
         | q < maxPoolSize -> [maxPoolSize, q + 1]
         | otherwise -> maxPoolSize : pools maxPoolSize q
-- maybe use remainder to partially pool the image later

edgeConv = [[  0.0, 1.0,  0.0],
            [ 1.0,  -4.0,  1.0],
            [  0.0, 1.0,  0.0]]

data Asciify = Asciify {
    uarg :: Bool
    ,lines :: Int
    ,targ :: Float
    ,filepath :: FilePath
    }
    deriving (Data,Typeable,Show,Eq)

main :: IO ()
main = do
  Asciify {..} <- cmdArgs $ Asciify
    {
      uarg = def &= help "Unclutter output"
      ,lines = 20 &= typ "20" &= help "Desired number of lines in the ouput"
      ,targ = 0.5 &= typ "0.5" &= help "0.0-1.0 less/more details"
      ,filepath = def &= argPos 0 &= typFile
    }

  dynImg <- readImage filepath
  case dynImg of
    Left err -> putStrLn err
    Right img -> do
      let greyscale = convertF img
      let (w, h) = (imageWidth greyscale, imageHeight greyscale)
      let ratio = fromIntegral w / fromIntegral h :: Float
      let dh = lines * asciifySize
      let dw = floor (fromIntegral lines * ratio) * asciifySize
      let poolsW = pools maxPoolSize (quot w dw)
      let poolsH = pools maxPoolSize (quot h dh)
      let processList = threshold targ : map pooling (zip poolsW poolsH)

      let processAndOutput (i, acc) f = do
            let processed = f acc
            -- saveBmpImage (filepath ++ "_processed" ++ show i ++ ".bmp") (ImageYF processed)
            return (i + 1, processed)

      (i, processed) <- foldM processAndOutput (0, greyscale) (reverse processList)

      putStrLn ("Image size: " ++ show (imageWidth processed, imageHeight processed))
      when (imageWidth processed > dw || imageHeight processed > dh) $ do
        error ("Incorrect image size. " ++
                "Wanted: " ++ show (dw, dh) ++
                ", got: " ++ show (imageWidth processed, imageHeight processed))

      -- debugging
      -- let withBoxes = boxify (2, 4) (fToRGBF processed)
      -- saveBmpImage (filepath ++ "_boxed.bmp") (ImageRGBF withBoxes)

      let asciified = asciify processed
      mapM_ putStrLn (if uarg then unclutter asciified else asciified)
