{-# LANGUAGE OverloadedStrings #-}

module Set2 where

import           Cipher
import           Control.Monad
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as CS
import           Data.Int
import           Data.Text (Text)
import qualified Data.Text as T
import           DefaultImports
import           FakeWeb
import           Oracle
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
                     , elideOutput $ CS.unpack dec
                     ]
  where
    filename = "data/set2.10.txt"


challenge11 :: IO ()
challenge11 = do
  tests <- replicateM 1000 verifyDetection
  let correct = length $ filter id tests
      wrong = length tests - correct
      accuracy = fromIntegral correct / fromIntegral (length tests)
  putStrLn $ unlines [ "  Challenge 11:"
                     , "    Correct Guesses:   " ++ show correct
                     , "    Incorrect Guesses: " ++ show wrong
                     , "    Accuracy:          " ++ show accuracy
                     , if accuracy > (99 / 100)
                       then "      Passed"
                       else "      FAILED"
                     ]
  where
    verifyDetection = do
      (cType, cFn) <- randomCipher
      encrypter <- randomEncrypter cFn
      let guess = detectCipher encrypter
      return (cType == guess)


challenge12 :: IO ()
challenge12 = do
  bytes <- decodeBase64File "data/set2.12.txt"
  key <- randomKey
  let cipher = prependingEncrypter key bytes
      bsize = detectECBBlockSize cipher
      dec = forceDecryptECB cipher
  putStrLn $ unlines [ "  Challenge 12:"
                     , "    Blocksize: " ++ show bsize
                     , "    Decrypted:"
                     , CS.unpack dec
                     ]


textShow :: Show a => a -> Text
textShow = T.pack . show

putTextLn :: Text -> IO ()
putTextLn = putStrLn . T.unpack

challenge13 :: IO ()
challenge13 = do
  let testQuery = "email=bob@dole.banana&uid=42&role=admin"
  putTextLn $ T.unlines [ "  Challenge 13:"
                        , "    Test: " ++ testQuery
                        , "    Parsed: " ++ (textShow . parseQueryString $ testQuery)
                        , "    profileFor: " ++ profileFor testQuery
                        ]

run :: IO ()
run = do
  putStrLn "Set 2:"
  challenge9
  challenge10
  challenge11
  challenge12
  challenge13
