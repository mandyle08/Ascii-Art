-- convertArray :: ([(Int, Int, Int)], Int, Int) -> [[(Int,Int,Int)]]
-- convertArray is the function I have written to address requirement (a)

-- whitePixels :: [[(Int,Int,Int)]] -> [[(Int,Int,Int)]]
-- whitePixels is the function I have written to address requirement (b)

-- whiteArray :: [(Int,Int,Int)] -> [(Int,Int,Int)]
-- whiteArray is a helper function I have written from whitePixels

--showAsASCIIArt' :: [[(Int,Int,Int)]] -> [[Char]]
--showAsASCIIArt' is the function I have written to address requirement (c)

--showAsc :: [(Int,Int,Int)] -> [Char]
-- showAsc is a helper function I have written from showAsASCIIART'

import Codec.BMP
import GHC.Word
import Data.ByteString

-- For this code to work you will need to have installed the package "bmp-1.2.6.3"

loadBitmapIntoIt :: FilePath -> IO ([(Int, Int, Int)], Int, Int)
loadBitmapIntoIt filename = do
  (Right bmp) <- readBMP filename
  return ((parseIntoRGBVals (convertToIntList (unpack (unpackBMPToRGBA32 bmp)))), (fst (bmpDimensions bmp)), (snd (bmpDimensions bmp)))

convertToIntList :: [Word8] -> [Int]
convertToIntList [] = []
convertToIntList (h:t) = (fromIntegral (toInteger h)) : (convertToIntList t)

parseIntoRGBVals :: [Int] -> [(Int, Int, Int)]
parseIntoRGBVals [] = []
parseIntoRGBVals (h:i:j:_:t) = (h,i,j) : (parseIntoRGBVals t)

convertArray :: ([(Int, Int, Int)], Int, Int) -> [[(Int,Int,Int)]]
convertArray ([],_,_) =[]
convertArray ((list), x, y)
  | y <= 0 = []
  | otherwise = (Prelude.drop (Prelude.length list - x) list) : convertArray((Prelude.take (Prelude.length list - x) list), x, y-1)

whitePixels :: [[(Int,Int,Int)]] -> [[(Int,Int,Int)]]
whitePixels [] = []
whitePixels (h:t) = whiteArray(h) : whitePixels(t)

whiteArray :: [(Int,Int,Int)] -> [(Int,Int,Int)]
whiteArray [] = []
whiteArray (h:t)
  | h == (0,0,0) = (0,0,0) : whiteArray(t)
  | h /= (0,0,0) = (255,255,255) : whiteArray(t)

showAsASCIIArt :: [[(Int, Int, Int)]] -> IO ()
showAsASCIIArt pixels = Prelude.putStr (unlines (showAsASCIIArt' pixels) )

showAsASCIIArt' :: [[(Int,Int,Int)]] -> [[Char]]
showAsASCIIArt' [] = []
showAsASCIIArt' (h:t) = showAsc(h) : showAsASCIIArt'(t)

showAsc :: [(Int,Int,Int)] -> [Char]
showAsc [] = []
showAsc (h:t)
  | h == (0,0,0) = ' ' : showAsc(t)
  | h == (255,255,255) = '█': showAsc(t)
