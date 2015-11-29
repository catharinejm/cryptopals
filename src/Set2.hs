module Set2 where

import           Cipher
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as CS
import           Types


challenge9 :: IO ()
challenge9 = print chal
  where
    chal = Challenge 9 expected actual
    input = CS.pack "YELLOW SUBMARINE"
    expected = "YELLOW SUBMARINE\x04\x04\x04\x04"
    actual = CS.unpack $ pkcs7Pad 20 input


challenge10 :: IO ()
challenge10 =


run :: IO ()
run = do
  putStrLn "Set 2:"
  challenge9
