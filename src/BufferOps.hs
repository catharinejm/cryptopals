{-# LANGUAGE ExistentialQuantification #-}

module BufferOps where

import           Control.Monad
import           Control.Monad.Writer.Lazy
import           Data.Binary.Get
import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Maybe
import           Data.Word


combineBuffers :: (Word8 -> Word8 -> a) -> (ByteString, ByteString) -> [a]
combineBuffers f = map (uncurry f) . uncurry BS.zip


xorBuffers :: ByteString -> ByteString -> ByteString
xorBuffers = curry $ BS.pack . combineBuffers xor


repeatingKeyXor :: ByteString -> ByteString -> ByteString
repeatingKeyXor key buf = xorBuffers buf (BS.cycle key)


hammingDistance :: ByteString -> ByteString -> Int
hammingDistance = curry $ sum . map popCount . combineBuffers xor


sliceBuffer :: Int -> Int -> ByteString -> ByteString
sliceBuffer cnt idx buf = BS.pack $ runGet (execWriterT doSlice) buf
  where
    doSlice = do
      s <- lift $ getUpto cnt []
      case listToMaybe $ drop idx s of
       Nothing -> return ()
       Just b -> tell [b] >> doSlice
    getUpto n bs = do
      empty <- isEmpty
      if n == 0 || empty
        then return $ reverse bs
        else do b <- getWord8
                getUpto (n-1) (b:bs)
