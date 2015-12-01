module Cipher where

import           BufferOps
import           Control.Monad.RWS
import qualified Crypto.Cipher.AES as AES
import           Data.Binary.Put
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


blockMap :: Int64 -> (ByteString -> ByteString) -> ByteString -> ByteString
blockMap bsize f buf = runPut $ perform $ groupedBuffer blockLength buf
  where
    perform :: [ByteString] -> Put
    perform [] = putByteString (f $ pkcs7Pad BS.empty)
    perform [b] | BS.length b < blockLength = putByteString (f $ pkcs7Pad b)
    perform (b:bs) = do
      putByteString $ f b
      perform bs


encryptBlock :: AESKey -> ByteString -> ByteString
encryptBlock key = BS.fromStrict . AES.encryptECB key . BS.toStrict


encryptECB :: AESKey -> ByteString -> ByteString
encryptECB key = blockMap blockLength encryptBlock


pkcs7Pad :: Int64 -> ByteString -> ByteString
pkcs7Pad len buf = padded
  where
    bufLen = BS.length buf
    padLen = len - bufLen `mod` len
    pad = BS.take padLen $ BS.repeat $ fromIntegral padLen
    padded = if bufLen < len then BS.concat [buf, pad] else buf


encryptCBC :: AESKey -> ByteString -> ByteString -> ByteString
encryptCBC key iv buf = BS.concat $ encrypt iv buf
  where
    encrypt l b | BS.null b = pkcs7Pad
    encrypt l b = let block = pkcs7Pad blockLength $ BS.take blockLength b
                      enc = encryptChunk l block
                  in enc : encrypt enc (BS.drop blockLength b)
    encryptChunk last cur = encryptECB key $ xorBuffers last cur


decryptCBC :: AESKey -> ByteString -> ByteString -> ByteString
decryptCBC key iv buf = BS.concat $ decrypt iv buf
  where
    decrypt l b | BS.null b = []
    decrypt l b = let chunk = BS.take blockLength b
                  in decryptChunk l chunk : decrypt chunk (BS.drop blockLength b)
    decryptChunk l b = xorBuffers l $ decryptECB key b
