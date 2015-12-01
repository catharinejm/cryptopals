module Cipher where

import           BufferOps
import qualified Crypto.Cipher.AES as AES
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Types

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
encryptCBC key iv buf = BS.concat $ encrypt iv buf
  where
    bufLen = 16
    encrypt l b | BS.null b = []
    encrypt l b = let enc = encryptChunk l (BS.take bufLen b)
                  in enc : encrypt enc (BS.drop bufLen b)
    encryptChunk last cur = encryptECB key $ xorBuffers last cur


decryptCBC :: AESKey -> ByteString -> ByteString -> ByteString
decryptCBC key iv buf = BS.concat $ decrypt iv buf
  where
    bufLen = 16
    decrypt l b | BS.null b = []
    decrypt l b = let chunk = BS.take bufLen b
                  in decryptChunk l chunk : decrypt chunk (BS.drop bufLen b)
    decryptChunk l b = xorBuffers l $ decryptECB key b
