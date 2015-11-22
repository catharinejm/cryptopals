module Set1 where

import TextEncodings
import BufferOps
import Control.Monad.Writer.Lazy
import Data.List

data Challenge = Challenge { chalNum :: Int
                           , chalExpected :: String
                           , chalActual :: String
                           }

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

printChallenge :: Challenge -> IO ()
printChallenge chal = do
  putStrLn $ "  Challenge " ++ show (chalNum chal) ++ ":"
  putStrLn $ "    Expected: " ++ chalExpected chal
  putStrLn $ "    Actual:   " ++ chalActual chal
  if (chalExpected chal == chalActual chal)
    then putStrLn "      Passed\n"
    else putStrLn "      FAILED\n"


allChallenges :: [Challenge]
allChallenges = [ challenge1
                , challenge2
                ]

run :: IO ()
run = do
  putStrLn "Set 1:"
  mapM_ printChallenge allChallenges
