module Set2 where

import           Cipher
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as CS
import           Types
import           Utils


challenge9 :: IO ()
challenge9 = print chal
  where
    chal = Challenge 9 expected actual
    input = CS.pack "YELLOW SUBMARINE"
    expected = "YELLOW SUBMARINE\x04\x04\x04\x04"
    actual = CS.unpack $ pkcs7Pad 20 input


challenge10 :: IO ()
challenge10 = do
  bytes <- decodeBase64File filename
  let dec = decryptCBC (initAES $ CS.pack "YELLOW SUBMARINE") (BS.pack $ take 16 $ repeat 0) bytes
  putStrLn $ unlines [ "  Challenge 10:"
                     , "    Decrypted:"
                     , truncateOutput $ CS.unpack dec
                     ]
  where
    filename = "data/set2.10.txt"


run :: IO ()
run = do
  putStrLn "Set 2:"
  challenge9
  challenge10
