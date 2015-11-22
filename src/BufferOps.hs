{-# LANGUAGE ExistentialQuantification #-}

module BufferOps where

import Control.Monad
import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Word

xorBuffers :: BS.ByteString -> BS.ByteString -> BS.ByteString
xorBuffers bs1 bs2 =
  BS.pack $ foldr (\(b1,b2) l -> (b1 `xor` b2) : l) [] $ BS.zip bs1 bs2
