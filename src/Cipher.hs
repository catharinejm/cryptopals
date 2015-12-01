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
decryptECB key = pkcs7Unpad16 . decryptBlock key


encryptBlock :: AESKey -> ByteString -> ByteString
encryptBlock key = BS.fromStrict . AES.encryptECB key . BS.toStrict


encryptECB :: AESKey -> ByteString -> ByteString
encryptECB key = encryptBlock key . pkcs7Pad16


pkcs7Pad :: Int64 -> ByteString -> ByteString
pkcs7Pad len buf = padded
  where
    bufLen = BS.length buf
    padLen = len - bufLen `mod` len
    pad = BS.take padLen $ BS.repeat $ fromIntegral padLen
    padded = BS.concat [buf, pad]


pkcs7Pad16 :: ByteString -> ByteString
pkcs7Pad16 = pkcs7Pad blockLength


pkcs7Unpad :: Int64 -> ByteString -> ByteString
pkcs7Unpad len buf = if padLen > len || len > bufLen
                     then error "Invalid PKCS#7 padding"
                     else BS.take (bufLen - padLen) buf
    bufLen = BS.length buf
    padLen = fromIntegral $ BS.last buf


pkcs7Unpad16 :: ByteString -> ByteString
pkcs7Unpad16 = pkcs7Unpad blockLength


takeBlock :: ByteString -> ByteString
takeBlock = BS.take blockLength


dropBlock :: ByteString -> ByteString
dropBlock = BS.drop blockLength


splitAtBlock :: ByteString -> (ByteString, ByteString)
splitAtBlock = juxt takeBlock dropBlock


encryptCBC :: AESKey -> ByteString -> ByteString -> ByteString
encryptCBC key iv buf = BS.concat $ encrypt iv buf
  where
    encrypt l b = let cur = takeBlock b
                  in if BS.length cur < blockLength
                     then [encryptChunk l $ pkcs7Pad16 cur]
                     else let enc = encryptChunk l cur
                          in enc : encrypt enc (dropBlock b)
    encryptChunk last = encryptBlock key . xorBuffers last


decryptCBC :: AESKey -> ByteString -> ByteString -> ByteString
decryptCBC key iv buf = BS.concat $ decrypt iv buf
  where
    validate b = if BS.length b < blockLength
                 then error "Premature end of encrypted block"
                 else b
    decrypt l b = let cur = validate $ takeBlock b
                      dec = decryptChunk l cur
                      next = dropBlock b
                  in if BS.null next
                     then [pkcs7Unpad16 dec]
                     else dec : decrypt cur next
    decryptChunk l = xorBuffers l . decryptBlock key
