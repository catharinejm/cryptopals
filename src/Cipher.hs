module Cipher where

import qualified Crypto.Cipher.AES as AES
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS


initAES :: BS.ByteString -> AES.AES
initAES = AES.initAES . BS.toStrict


decryptECB :: AES.AES -> ByteString -> ByteString
decryptECB key = BS.fromStrict . AES.decryptECB key . BS.toStrict


pkcs7Pad :: Int -> ByteString -> ByteString
pkcs7Pad len buf = BS.concat [buf, pad]
  where
    bufLen = fromIntegral $ BS.length buf
    padLen = len - bufLen
    pad = if len > bufLen
          then BS.pack $ take padLen $ repeat $ fromIntegral padLen
          else BS.empty
