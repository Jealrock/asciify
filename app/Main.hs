module Main where
import Codec.Picture

main :: IO ()
main = putStrLn "Hello, Haskell!"

dynSquare :: DynamicImage -> DynamicImage
dynSquare = dynamicPixelMap squareImage

squareImage :: Pixel a => Image a -> Image a
squareImage img = generateImage (\x y -> pixelAt img x y) edge edge
    where edge = min (imageWidth img) (imageHeight img)
