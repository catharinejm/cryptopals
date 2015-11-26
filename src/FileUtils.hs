module FileUtils where

import System.IO
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Writer
import Data.Word

import TextEncodings
import BufferOps

findOneByteFileKey :: FilePath -> IO Word8
findOneByteFileKey filename =
  liftM fst $ withFile filename ReadMode search
  where
    search h = liftM (findOneByteKey . fromHex) $ hGetLine h

decodeHexFile :: FilePath -> IO ByteString
decodeHexFile filename =
  withFile filename ReadMode decode
  where
    decode h = execWriterT (decodeWriter h)
    decodeWriter :: Handle -> WriterT ByteString IO ()
    decodeWriter h = do
      eof <- lift $ hIsEOF h
      if eof
        then return ()
        else do line <- lift (hGetLine h)
                let bs = fromHex line
                tell bs
                decodeWriter h
