{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}

module Main where
import System.Environment
import System.Console.CmdArgs
import System.Exit
import Control.Monad (when, foldM)
import Asciify.Picture
import Asciify.Text

asciifySize = 4
desiredSize = 20
maxPoolSize = 4

pools :: Int -> Int -> [(Int, Int)]
pools maxPoolSize poolBy = case quotRem poolBy maxPoolSize of
  (q, r) | q == 0 && r == 0 -> []
         | q == 0 -> [(r,r)]
         | q < maxPoolSize -> [(maxPoolSize, maxPoolSize), (q + 1, q + 1)]
         | otherwise -> (maxPoolSize, maxPoolSize) : pools maxPoolSize q
-- maybe use remainder to partially pool the image later

edgeConv = [[  0.0, 1.0,  0.0],
            [ 1.0,  -4.0,  1.0],
            [  0.0, 1.0,  0.0]]

data Asciify = Asciify {
    uarg :: Bool
    ,targ :: Float
    ,filepath :: FilePath
    }
    deriving (Data,Typeable,Show,Eq)

main :: IO ()
main = do
  Asciify {..} <- cmdArgs $ Asciify
    {
      uarg = def &= help "Unclutter output"
      ,targ = 0.5 &= typ "0.5" &= help "0.0-1.0 less/more details"
      ,filepath = def &= argPos 0 &= typFile
    }

  dynImg <- readImage filepath
  case dynImg of
    Left err -> putStrLn err
    Right img -> do
      let greyscale = convertF img
      let w = imageWidth greyscale -- TODO: should be biggest size
      let poolBy = quot w (desiredSize * asciifySize)
      let processList = threshold targ : map pooling (pools maxPoolSize poolBy)

      let processAndOutput (i, acc) f = do
            let processed = f acc
            -- saveBmpImage (filename ++ "_processed" ++ show i ++ ".bmp") (ImageYF processed)
            return (i + 1, processed)

      (i, processed) <- foldM processAndOutput (0, greyscale) (reverse processList)

      putStrLn ("Image size: " ++ show (imageWidth processed, imageHeight processed))
      when (imageWidth processed > desiredSize * asciifySize ||
            imageHeight processed > desiredSize * asciifySize) $ do
        error ("Incorrect image size. " ++
                "Wanted: " ++ show (desiredSize * asciifySize, desiredSize * asciifySize) ++
                ", got: " ++ show (imageWidth processed, imageHeight processed))

      -- debugging
      -- let withBoxes = boxify (2, 4) (fToRGBF processed)
      -- saveBmpImage (filename ++ "_boxed.bmp") (ImageRGBF withBoxes)

      let asciified = asciify processed
      mapM_ putStrLn (if uarg then unclutter asciified else asciified)
