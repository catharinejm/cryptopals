module Cipher where

import           BufferOps
import           Control.Monad.State
import qualified Crypto.Cipher.AES as AES
import           Data.Binary.Put
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Int
import           Types


blockLength :: Int64
blockLength = 16


initAES :: ByteString -> AESKey
initAES = AES.initAES . BS.toStrict


decryptBlock :: AESKey -> ByteString -> ByteString
decryptBlock key = BS.fromStrict . AES.decryptECB key . BS.toStrict


decryptECB :: AESKey -> ByteString -> ByteString
decryptECB key = pkcs7Unpad blockLength . decryptBlock key


encryptBlock :: AESKey -> ByteString -> ByteString
encryptBlock key = BS.fromStrict . AES.encryptECB key . BS.toStrict


encryptECB :: AESKey -> ByteString -> ByteString
encryptECB key = encryptBlock key . pkcs7Pad blockLength


pkcs7Pad :: Int64 -> ByteString -> ByteString
pkcs7Pad len buf = padded
  where
    bufLen = BS.length buf
    padLen = len - bufLen `mod` len
    pad = BS.take padLen $ BS.repeat $ fromIntegral padLen
    padded = BS.concat [buf, pad]


pkcs7Unpad :: Int64 -> ByteString -> ByteString
pkcs7Unpad len buf = BS.take (len - padLen) buf
  where
    padLen = fromIntegral $ BS.last buf


encryptCBC :: AESKey -> ByteString -> ByteString -> ByteString
encryptCBC key iv buf = BS.concat $ encrypt iv buf
  where
    pad = pkcs7Pad blockLength
    encrypt l b = let cur = BS.take blockLength b
                  in if BS.length cur < blockLength
                     then [encryptChunk l $ pad cur]
                     else let enc = encryptChunk l $ cur
                          in enc : encrypt enc (BS.drop blockLength b)
    encryptChunk last = encryptBlock key . xorBuffers last


decryptCBC :: AESKey -> ByteString -> ByteString -> ByteString
decryptCBC key iv buf = BS.concat $ decrypt iv buf
  where
    validate b = if BS.length b < blockLength
                 then error "Premature end of encrypted block"
                 else b
    decrypt l b = let cur = validate $ BS.take blockLength b
                      dec = decryptChunk l cur
                      next = BS.drop blockLength b
                  in if BS.null next
                     then [pkcs7Unpad blockLength dec]
                     else dec : decrypt cur (BS.drop blockLength b)
    decryptChunk l = xorBuffers l . decryptBlock key
