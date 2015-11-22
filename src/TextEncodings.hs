module TextEncodings where

import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Binary.Get
import           Data.Binary.Bits.Get (BitGet)
import qualified Data.Binary.Bits.Get as B
import qualified Data.Binary.Bits.Put as B
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
decodeHex hexStr = B.runBitPut $ decode padded
  where
    padded = if odd $ length hexStr then '0':hexStr else hexStr
    putHex n = fromHex n >>= B.putWord8 4
    decode [] = return ()
    decode (n1:n2:rest) = putHex n1 >> putHex n2 >> decode rest
    fromHex n | isDigit n    = return $ fromIntegral $ (ord n) - (ord '0')
    fromHex n | isHexDigit n = return $ fromIntegral $ (ord $ toLower n) - (ord 'a') + 10
    fromHex n = fail $ "invalid hex char " ++ show n ++ " in hex string: " ++ hexStr

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


decodeBase64 :: String -> Put
decodeBase64 b64 = B.runBitPut (decode b64)
  where
    toByte '=' = fail $ "Illegally placed '=' in base64 string: " ++ b64
    toByte c | c >= 'A' && c <= 'Z' = return $ fromIntegral $ ord c - ord 'A'
    toByte c | c >= 'a' && c <= 'z' = return $ fromIntegral $ ord c - ord 'a' + 26
    toByte c | c >= '0' && c <= '9' = return $ fromIntegral $ ord c - ord '0' + 52
    toByte '+' = return 62
    toByte '/' = return 63
    toByte c = fail $ "Illegal base64 char " ++ show c ++ " in base64 string: " ++ b64
    putB64 c = toByte c >>= B.putWord8 6
    decode [] = return ()
    decode [_] = fail $ "Irregularly truncated base64 string: " ++ b64
    decode [c1, c2] = putB64 c1 >> putB64 c2
    decode [c1, c2, c3] = putB64 c1 >> putB64 c2 >> putB64 c3
    decode [c1, c2, '=', '='] = putB64 c1 >> putB64 c2
    decode [c1, c2, c3, '='] = putB64 c1 >> putB64 c2 >> putB64 c3
    decode (c1:c2:c3:c4:rest) = putB64 c1 >> putB64 c2 >> putB64 c3 >> putB64 c4 >> decode rest


toBase64 :: ByteString -> String
toBase64 bs = let (_, b64str) = runGet (evalRWST encodeBase64 () (0,0)) bs in b64str

fromBase64 :: String -> ByteString
fromBase64 b64 = runPut $ decodeBase64 b64

hexToBase64 :: String -> String
hexToBase64 = toBase64 . fromHex

base64ToHex :: String -> String
base64ToHex = toHex . fromBase64
