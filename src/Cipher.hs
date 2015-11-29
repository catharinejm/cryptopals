module Cipher where

import           BufferOps
import qualified Crypto.Cipher.AES as AES
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

initAES :: BS.ByteString -> AESKey
initAES = AES.initAES . BS.toStrict


decryptECB :: AESKey -> ByteString -> ByteString
decryptECB key = BS.fromStrict . AES.decryptECB key . BS.toStrict


encryptECB :: AESKey -> ByteString -> ByteString
encryptECB key = BS.fromStrict . AES.encryptECB key . BS.toStrict


pkcs7Pad :: Int -> ByteString -> ByteString
pkcs7Pad len buf = BS.concat [buf, pad]
  where
    bufLen = fromIntegral $ BS.length buf
    padLen = len - bufLen
    pad = if len > bufLen
          then BS.pack $ take padLen $ repeat $ fromIntegral padLen
          else BS.empty


encryptCBC :: AESKey -> ByteString -> ByteString -> ByteString
encryptCBC key iv buf =
  
