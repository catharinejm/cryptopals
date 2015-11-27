{-# LANGUAGE ExistentialQuantification #-}

module BufferOps where

import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Word


combineBuffers :: (Word8 -> Word8 -> a) -> (ByteString, ByteString) -> [a]
combineBuffers f = map (uncurry f) . uncurry BS.zip


xorBuffers :: ByteString -> ByteString -> ByteString
xorBuffers = curry $ BS.pack . combineBuffers xor
      
    
repeatingKeyXor :: ByteString -> ByteString -> ByteString
repeatingKeyXor key buf = xorBuffers buf (BS.cycle key)


hammingDistance :: ByteString -> ByteString -> Int
hammingDistance = curry $ sum . map popCount . combineBuffers xor
