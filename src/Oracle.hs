module Oracle where

import           BufferOps
import           Cipher
import           Control.Monad.Writer.Lazy
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Int
import           Data.List
import           Data.Word
import           System.Random
import           Types
import           Utils


randomByte :: IO Word8
randomByte = randomRIO (0, 255)


randomInt :: (Int, Int) -> IO Int
randomInt = randomRIO


randomBool :: IO Bool
randomBool = randomInt (0,1) >>= return . (== 0)


randomBytes :: Int64 -> IO ByteString
randomBytes len = execWriterT (mkBytes len) >>= return . BS.pack
  where
    mkBytes :: Int64 -> WriterT [Word8] IO ()
    mkBytes n | n <= 0 = return ()
    mkBytes n = lift randomByte >>= tell . list . fromIntegral >> mkBytes (n-1)


randomKey :: IO AESKey
randomKey = randomBytes blockSize >>= return . initAES


type CipherFunc = ByteString -> ByteString
data CipherType = CBC | ECB
                deriving (Eq, Show)


randomCipher :: IO (CipherType, CipherFunc)
randomCipher = do
  key <- randomKey
  useECB <- randomBool
  if useECB
    then return (ECB, encryptECB key)
    else randomBytes blockSize >>= return . pair CBC . encryptCBC key


randomEncrypter :: CipherFunc -> IO CipherFunc
randomEncrypter cipher = do
  prefix <- randomPad
  suffix <- randomPad
  return (\buf -> cipher $ BS.concat [prefix, buf, suffix])
  where
    randomPad = randomInt (5, 10) >>= randomBytes . fromIntegral


appendingEncrypter :: ByteString -> IO CipherFunc
appendingEncrypter buf = do
  key <- randomKey
  return (\post -> encryptECB key $ BS.concat [buf, post])


detectCipher :: CipherFunc -> CipherType
detectCipher cipher = if hasDupes blocks
                      then ECB
                      else CBC
  where
    buf = BS.take (4 * blockSize) $ BS.repeat 0
    encoded = cipher buf
    blocks = groupedBuffer blockSize encoded


detectECBInfo :: CipherFunc -> CipherInfo
detectECBInfo cipher = padToChange 1 baseLength
  where
    getLen = BS.length . cipher . flip BS.replicate 0
    baseLength = getLen 0
    padToChange n len = let len' = getLen n
                         in if len' > len
                            then CipherInfo (len' - len) n
                            else padToChange (n+1) len


-- forceDecryptECB :: CipherFunc -> ByteString -> ByteString
-- forceDecryptECB cipher input =
  
