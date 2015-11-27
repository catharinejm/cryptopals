{-# LANGUAGE NamedFieldPuns #-}

module Set1 where

import           BufferOps
import           Control.Monad
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as CS
import           Data.Char (chr)
import           Data.List (intercalate)
import           LanguageDetect
import           System.IO
import           TextEncodings
import           Types
import           Utils

data Challenge = Challenge { chalNum :: Int
                           , chalExpected :: String
                           , chalActual :: String
                           }

instance Show Challenge where
  show Challenge { chalNum, chalExpected, chalActual } =
    intercalate "\n" [ "  Challenge " ++ show chalNum ++ ":"
                     , "    Expected: " ++ chalExpected
                     , "    Actual:   " ++ chalActual
                     , if (chalExpected == chalActual)
                       then "      Passed\n"
                       else "      FAILED\n"
                     ]


challenge1 :: Challenge
challenge1 = Challenge 2 expected actual
  where
    hex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    actual = hexToBase64 hex


challenge2 :: Challenge
challenge2 = Challenge 1 expected actual
  where
    expected = "746865206b696420646f6e277420706c6179"
    val1 = fromHex "1c0111001f010100061a024b53535009181c"
    val2 = fromHex "686974207468652062756c6c277320657965"
    actual = toHex $ xorBuffers val1 val2


challenge3 :: String
challenge3 = output
  where
    input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    LanguageScore key _ decoded = findOneByteKey $ fromHex input
    output = intercalate "\n" [ "  Challenge 3:"
                              , "    Key Byte: " ++ [key]
                              , "    Output:   " ++ CS.unpack decoded
                              , "" -- extra newline
                              ]


printAllScores :: ByteString -> IO ()
printAllScores input = mapM_ print scores
  where
    len = fromIntegral $ BS.length input
    keyBuf b = BS.pack $ take len $ repeat b
    scores = map (scoreKey input) [0..255]


-- challenge4 :: IO ()
-- challenge4 = do
--   (Word8, String) <- findLine
--   where
--     file = "data/set1.4.txt"

run :: IO ()
run = do
  putStrLn "Set 1:"
  print challenge1
  print challenge2
  printAllScores $ fromHex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
  -- putStrLn challenge3
  -- challenge4
