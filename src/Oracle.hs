{-# LANGUAGE NamedFieldPuns, FlexibleContexts #-}

module Oracle where

import           BufferOps
import           Cipher
import           Control.Monad.RWS.Lazy
import           Control.Monad.Writer.Lazy
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Int
import           Data.Map ((!))
import qualified Data.Map as M
import           Data.Word
import           DefaultImports
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


prependingEncrypter :: AESKey -> ByteString -> CipherFunc
prependingEncrypter key buf = \pre -> encryptECB key $ BS.concat [pre, buf]


detectCipher :: CipherFunc -> CipherType
detectCipher cipher = if hasDupes blocks
                      then ECB
                      else CBC
  where
    buf = BS.take (4 * blockSize) $ BS.repeat 0
    encoded = cipher buf
    blocks = groupedBuffer blockSize encoded


buildDecryptEnv :: CipherFunc -> DecryptEnv
buildDecryptEnv cipher = DecryptEnv cipher blockSize (baseLength - padLen)
  where
    getLen = BS.length . cipher . flip BS.replicate 0
    baseLength = getLen 0
    padToChange n len = let len' = getLen n
                         in if len' > len
                            then (len' - len, n - 1)
                            else padToChange (n+1) len
    (blockSize, padLen) = padToChange 1 baseLength


detectECBBlockSize :: CipherFunc -> Int64
detectECBBlockSize = deBlockSize . buildDecryptEnv


data DecryptEnv = DecryptEnv { deCipher    :: CipherFunc
                             , deBlockSize :: !Int64
                             , deInputLen  :: !Int64
                             }
data Decrypter = Decrypter { dcKnownBytes :: !ByteString
                           , dcBlockNum   :: !Int64
                           , dcPadding    :: !ByteString
                           , dcBytesRead  :: !Int64
                           }


forceDecryptECB :: CipherFunc -> ByteString
forceDecryptECB cipher = BS.pack bytes
  where
    bytes = snd $ execRWS runForceDecrypt env state
    env @ DecryptEnv { deBlockSize = bsize } = buildDecryptEnv cipher
    bufLen = fromIntegral (BS.length $ cipher BS.empty)
    state = Decrypter (BS.replicate (bsize - 1) 0) 0 (BS.replicate (bsize - 1) 0) 0


type ForceDecrypt a = RWS DecryptEnv [Word8] Decrypter a

runForceDecrypt :: ForceDecrypt ()
runForceDecrypt = do
  il <- asks deInputLen
  br <- gets dcBytesRead
  if br == il - 1
    then return ()
    else do dict <- buildDict
            blk <- curBlock
            addByte (dict ! blk)
            alterPadding
            runForceDecrypt
  where
    curBlock = getBlock <$> blockNum <*> (cipher <*> padding)
    cipher = asks deCipher
    bsize = asks deBlockSize
    known = gets dcKnownBytes
    padding = gets dcPadding
    blockNum = gets dcBlockNum
    appendToKnown n = headBlock <$> (cipher <*> ((flip BS.snoc n) <$> known))
    buildDict = do
      keys <- mapM appendToKnown [0..255]
      pure $ M.fromList $ zip keys [0..255]
    addByte b = do
      modify $ \s -> s { dcKnownBytes = BS.tail $ BS.snoc (dcKnownBytes s) b
                       , dcBytesRead = (dcBytesRead s) + 1
                       }
      tell [b]
    alterPadding = do
      p <- padding
      if BS.null p
        then do bs <- bsize
                modify $ \s -> s { dcPadding = BS.replicate (bs - 1) 0 }
                nextBlock
        else modify $ \s -> s { dcPadding = BS.tail p }
    nextBlock = blockNum >>= \bn -> modify $ \s -> s { dcBlockNum = bn + 1 }
