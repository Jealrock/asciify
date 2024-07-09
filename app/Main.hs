module Main where
import System.Environment
import System.Console.GetOpt
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

options :: [OptDescr Float]
options =
  [ Option ['t'] ["threshold"] (ReqArg (\x -> read x :: Float) "0.5") "0.0 - 1.0, less/more details"
  ]

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    (flags, nonOpts, []) -> do
      let filename = head nonOpts
      let tharg = head flags

      dynImg <- readImage filename
      case dynImg of
        Left err -> putStrLn err
        Right img -> do
          let greyscale = convertF img
          let w = imageWidth greyscale -- TODO: should be biggest size
          let poolBy = quot w (desiredSize * asciifySize)

          -- let processList = map pooling (pools maxPoolSize poolBy) ++
          --                   [exposure (0.1, 10.0)]
          -- let processList = inverse : threshold 0.2 : convolution edgeConv : map pooling (pools maxPoolSize poolBy)
          putStrLn ("Threshold: " ++ (show tharg))
          let processList = threshold tharg : map pooling (pools maxPoolSize poolBy)

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

          mapM_ putStrLn (asciify processed)
    (_, _, errs) -> do
      putStrLn $ concat errs ++ usageInfo "Usage: program [OPTIONS]" options
      exitWith (ExitFailure 1)
