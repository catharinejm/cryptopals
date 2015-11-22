module Util where

import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Binary.Get
import           Data.Binary.Bits.Get (BitGet)
import qualified Data.Binary.Bits.Get as B
import           Data.Binary.Put
import           Data.Word
import           Data.Char
import           Data.List
import           Data.Vector ((!), (!?))
import qualified Data.Vector as V
import           Control.Monad
import           Control.Monad.RWS.Lazy
import           Control.Monad.Writer.Lazy
import           Control.Monad.Except

decodeHex :: String -> Put
decodeHex [] = return ()
decodeHex hexStr | odd $ length hexStr = decodeHex $ '0':hexStr
decodeHex hexStr = validate >> decode hexStr
  where
    validate = when (any (not . isHexDigit) hexStr) $ fail $ "invalid hex string: " ++ hexStr
    decode [] = return ()
    decode (n1:n2:rest) = do
      hi <- fromHex n1
      lo <- fromHex n2
      putWord8 $ fromIntegral $ hi `shiftL` 4 .|. lo
      decode rest
    fromHex n | isDigit n = return $ (ord n) - (ord '0')
    fromHex n | isHexDigit n = return $ (ord $ toLower n) - (ord 'a') + 10
    fromHex n = fail $ "invalid hex char: " ++ [n]

encodeHex :: WriterT String Get ()
encodeHex = do
  empty <- lift isEmpty
  if empty
    then return ()
    else do hs <- lift $ B.runBitGet hexStr
            tell hs
            encodeHex
  where
    hexStr = do hi <- B.getWord8 4
                lo <- B.getWord8 4
                return [(hexChars ! (fromIntegral hi)), (hexChars ! (fromIntegral lo))]
    hexChars = V.fromList $ ['0'..'9']++['a'..'f']

fromHex :: String -> ByteString
fromHex str = runPut $ decodeHex str

toHex :: ByteString -> String
toHex bs = runGet (execWriterT encodeHex) bs

encodeBase64 :: RWST () String (Word8, Int) Get ()
encodeBase64 = do
  empty <- lift isEmpty
  (lastBits, bitLen) <- get
  case (empty, bitLen) of
   (True, 0) -> pad
   (True, _) -> do tell $ b64str $ lastBits `shiftL` (6 - bitLen)
                   pad
   (_, 6)    -> do tell $ b64str lastBits
                   put (0,0)
                   encodeBase64
   _         -> do byte <- nextByte
                   let shft = 6 - bitLen
                       loBits = byte `shiftR` (8 - shft)
                       b64 = b64str $ (lastBits `shiftL` shft) .|. loBits
                       nextBits = byte `shiftL` shft `shiftR` shft
                       nextBitLen = 8 - shft
                   tell b64
                   put (nextBits, nextBitLen)
                   encodeBase64
  where
    nextByte = do
      (byte, len) <- get
      if len == 8
        then return byte
        else lift getWord8
    b64str n = [b64chars ! (fromIntegral n)]
    b64chars = V.fromList $ ['A'..'Z']++['a'..'z']++['0'..'9']++['+','/']
    pad = do
      nread <- lift bytesRead
      let padLen = mod (fromIntegral nread) 3
      tell $ take padLen (repeat '=')
         
toBase64 :: ByteString -> String
toBase64 bs = let (_, b64str) = runGet (evalRWST encodeBase64 () (0,0)) bs in b64str

hexToBase64 :: String -> String
hexToBase64 = toBase64 . fromHex
