module Utils where

import           BufferOps
import           Control.Monad
import           Control.Monad.Writer
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
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
