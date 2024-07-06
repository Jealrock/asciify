module Main where
import System.Environment( getArgs )
import Control.Monad (when)
import Asciify.Picture
import Asciify.Text

asciifySize = 3
desiredSize = 20
maxPoolSize = 4

pools :: Int -> Int -> [(Int, Int)]
pools maxPoolSize poolBy = case quotRem poolBy maxPoolSize of
  (q, r) | q == 0 && r == 0 -> []
         | q == 0 -> [(r,r)]
         | q < maxPoolSize -> [(maxPoolSize, maxPoolSize), (q + 1, q + 1)]
         | otherwise -> (maxPoolSize, maxPoolSize) : pools maxPoolSize q
-- maybe use remainder to partially pool the image later

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
          let w = imageWidth greyscale -- TODO: should be biggest size
          let poolBy = quot w (desiredSize * asciifySize)

          let processList = map pooling (pools maxPoolSize poolBy) ++
                            [exposure (0.1, 10.0)]

          let processed = foldr (\f acc -> f acc) greyscale processList

          when (imageWidth processed >= desiredSize * asciifySize ||
                imageHeight processed >= desiredSize * asciifySize) $ do
            error ("Incorrect image size. " ++
                   "Wanted: " ++ show (desiredSize * asciifySize, desiredSize * asciifySize) ++
                   ", got: " ++ show (imageWidth processed, imageHeight processed))

          saveBmpImage (filename ++ "_processed.bmp") (ImageYF processed)
          -- debugging

          let withBoxes = boxify 3 (fToRGBF processed)
          saveBmpImage (filename ++ "_with_3boxes.bmp") (ImageRGBF withBoxes)
          let withBoxes = boxify 5 (fToRGBF processed)
          saveBmpImage (filename ++ "_with_5boxes.bmp") (ImageRGBF withBoxes)

          mapM_ putStrLn (asciify processed)
