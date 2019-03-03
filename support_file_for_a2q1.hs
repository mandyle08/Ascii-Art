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
convertArray ([],x,y)
            | y <= 0 = 

--showAsASCIIArt :: [[(Int, Int, Int)]] -> IO ()
--showAsASCIIArt pixels = Prelude.putStr (unlines (showAsASCIIArt' pixels) )

