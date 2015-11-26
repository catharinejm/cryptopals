{-# LANGUAGE NamedFieldPuns #-}

module Set1 where

import TextEncodings
import BufferOps
import FileUtils
import Data.List (intercalate)
import Data.Char (chr)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as CS

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
    (key, decoded) = findOneByteKey $ fromHex input
    output = intercalate "\n" [ "  Challenge 3:"
                              , "    Key Byte: " ++ (show $ chr $ fromIntegral key)
                              , "    Output:   " ++ decoded
                              , "" -- extra newline
                              ]

challenge4 :: IO ()
challenge4 = do
  key <- findOneByteFileKey "data/set1.4.txt"
  bytes <- decodeHexFile "data/set1.4.txt"
  let decoded = xorBuffers bytes $ BS.pack $ (take $ fromIntegral $ BS.length bytes) $ repeat key
  putStrLn   "  Challenge 4:"
  putStrLn $ "    Key Byte: " ++ (show $ chr $ fromIntegral key)
  putStrLn   "    Output:"
  putStrLn $ CS.unpack decoded
  putStrLn ""

run :: IO ()
run = do
  putStrLn "Set 1:"
  print challenge1
  print challenge2
  putStrLn challenge3
  challenge4
