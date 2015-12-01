module Utils where

import           BufferOps
import           Control.Monad
import           Control.Monad.Writer
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.List
import qualified Data.Map as M
import           Data.Map.Strict (Map)
import           Data.Word
import           System.IO
import           TextEncodings


decodeFile :: (String -> ByteString) -> FilePath -> IO ByteString
decodeFile decoder filename =
  withFile filename ReadMode decode
  where
    decode h = execWriterT (decodeWriter h)
    decodeWriter :: Handle -> WriterT ByteString IO ()
    decodeWriter h = do
      eof <- lift $ hIsEOF h
      when (not eof) $ do
        line <- lift (hGetLine h)
        let bs = decoder line
        tell bs
        decodeWriter h


decodeHexFile :: FilePath -> IO ByteString
decodeHexFile = decodeFile fromHex


decodeBase64File :: FilePath -> IO ByteString
decodeBase64File = decodeFile fromBase64


frequencies :: Ord a => [a] -> Map a Int
frequencies lis = foldr (M.alter inc) M.empty lis
  where
    inc Nothing = Just 1
    inc (Just n) = Just $ n+1


minimumWith :: Ord b => (a -> b) -> [a] -> a
minimumWith f = minimumBy (\x y -> f x `compare` f y)


juxt :: (a -> b) -> (a -> c) -> a -> (b, c)
juxt f g x = (f x, g x)


juxt3 :: (a -> b) -> (a -> c) -> (a -> d) -> a -> (b, c, d)
juxt3 f g h x = (f x, g x, h x)


truncateOutput :: String -> String
truncateOutput input = unlines trunc
  where
    inLines = lines input
    trunc = if length inLines > 6
            then take 3 inLines ++ ["..."] ++ lastN 3 inLines
            else inLines
    lastN n l = drop (length l - n) l
