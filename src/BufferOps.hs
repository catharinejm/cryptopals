{-# LANGUAGE ExistentialQuantification #-}

module BufferOps where

import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS


xorBuffers :: ByteString -> ByteString -> ByteString
xorBuffers bs1 bs2 =
  BS.pack $ foldr (\(b1,b2) l -> (b1 `xor` b2) : l) [] $ BS.zip bs1 bs2
      
    
repeatingKeyXor :: ByteString -> ByteString -> ByteString
repeatingKeyXor key buf = xorBuffers buf (BS.cycle key)
