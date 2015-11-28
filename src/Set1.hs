{-# LANGUAGE NamedFieldPuns #-}

module Set1 where

import           BufferOps
import           Control.Monad
import           Control.Monad.Writer.Lazy
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as CS
import           Data.Char (chr)
import           Data.List (intercalate)
import           KeyDetect
import           System.CPUTime
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
    unlines [ "  Challenge " ++ show chalNum ++ ":"
            , "    Expected: " ++ chalExpected
            , "    Actual:   " ++ chalActual
            , if (chalExpected == chalActual)
              then "      Passed"
              else "      FAILED"
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
    output = unlines [ "  Challenge 3:"
                     , "    Key:    " ++ show (chr (fromIntegral key))
                     , "    Output: " ++ show decoded
                     ]


printAllScores :: ByteString -> IO ()
printAllScores input = mapM_ print scores
  where
    len = fromIntegral $ BS.length input
    keyBuf b = BS.pack $ take len $ repeat b
    scores = map (scoreKey input) [0..255]


challenge4 :: IO ()
challenge4 = do
  start <- getCPUTime
  (lineNum, ls@(LanguageScore key score decoded)) <- pickLine
  let _ = seq ls undefined
  end <- getCPUTime
  putStrLn $ unlines [ "  Challenge 4:"
                     , "    Line:   " ++ show (lineNum + 1)
                     , "    Key:    " ++ show (chr (fromIntegral key))
                     , "    Output: " ++ show decoded
                     , "    Time:   " ++ show ((fromIntegral (end - start)) / 1000000000) ++ "ms"
                     ]
  where
    filename = "data/set1.4.txt"
    pickLine = liftM bestScoreWithIndex scoreLines
    scoreLines = withFile filename ReadMode scoreLine
    scoreLine h = execWriterT doScore
      where
        doScore = do
          eof <- lift $ hIsEOF h
          when (not eof) $ do
            line <- lift $ hGetLine h
            tell [findOneByteKey $ fromHex line]
            doScore


challenge5 :: IO ()
challenge5 =
  print $ Challenge 5 expected actual
  where
    input = "Burning 'em, if you ain't quick and nimble\n" ++
            "I go crazy when I hear a cymbal"
    expected = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272" ++
               "a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
    actual = toHex $ repeatingKeyXor (CS.pack "ICE") (CS.pack input)
  

printKeyScores :: ByteString -> IO ()
printKeyScores buf = do
  let sizes = [2..40]
      scores = zip sizes $ map (scoreKeysize buf) sizes
  mapM_ print scores

challenge6 :: IO ()
challenge6 = do
  bytes <- decodeBase64File filename
  let key = findMultibyteKey bytes
      decoded = repeatingKeyXor key bytes
  putStrLn $ unlines [ "  Challenge 6:"
                     , "    Key: " ++ show key
                     , "    Decoded:"
                     , CS.unpack decoded
                     ]
  where
    filename = "data/set1.6.txt"


run :: IO ()
run = do
  putStrLn "Set 1:"
  print challenge1
  print challenge2
  putStrLn challenge3
  challenge4
  challenge5
  challenge6
