{-# LANGUAGE ExistentialQuantification #-}

module BufferOps where

import           Control.Monad
import           Control.Monad.Writer.Lazy
import           Data.Binary.Get
import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Int
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


groupedBuffer :: Int64 -> ByteString -> [ByteString]
groupedBuffer 0 _ = []
groupedBuffer _ buf | BS.null buf = []
groupedBuffer n buf = BS.take n buf : groupedBuffer n (BS.drop n buf)


transposeBuffer :: Int64 -> ByteString -> [ByteString]
transposeBuffer n buf = BS.transpose $ groupedBuffer n buf
