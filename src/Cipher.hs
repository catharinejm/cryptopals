module Cipher where

import           BufferOps
import qualified Crypto.Cipher.AES as AES
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Int
import           Types


blockLength :: Int64
blockLength = 16


initAES :: BS.ByteString -> AESKey
initAES = AES.initAES . BS.toStrict


decryptECB :: AESKey -> ByteString -> ByteString
decryptECB key = BS.fromStrict . AES.decryptECB key . BS.toStrict


encryptECB :: AESKey -> ByteString -> ByteString
encryptECB key = BS.fromStrict . AES.encryptECB key . BS.toStrict


pkcs7Pad :: Int64 -> ByteString -> ByteString
pkcs7Pad len buf = BS.concat [buf, pad]
  where
    bufLen = BS.length buf
    padLen = len - bufLen `mod` len
    pad = BS.take padLen $ BS.repeat $ fromIntegral padLen


encryptCBC :: AESKey -> ByteString -> ByteString -> ByteString
encryptCBC key iv buf = BS.concat $ encrypt iv buf
  where
    encrypt l b | BS.null b = []
    encrypt l b = let enc = encryptChunk l (BS.take blockLength b)
                  in enc : encrypt enc (BS.drop blockLength b)
    encryptChunk last cur = encryptECB key $ xorBuffers last cur


decryptCBC :: AESKey -> ByteString -> ByteString -> ByteString
decryptCBC key iv buf = BS.concat $ decrypt iv buf
  where
    decrypt l b | BS.null b = []
    decrypt l b = let chunk = BS.take blockLength b
                  in decryptChunk l chunk : decrypt chunk (BS.drop blockLength b)
    decryptChunk l b = xorBuffers l $ decryptECB key b
