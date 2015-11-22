module Set1 where

import TextEncodings
import Control.Monad.Writer.Lazy
import Data.List

challenge1 :: WriterT [String] IO ()
challenge1 = do
  tell ["Expected: " ++ expected]
  tell ["Actual:   " ++ actual]
  when (expected == actual) $ tell ["Passed!"]
  when (expected /= actual) $ tell ["Failed!"]
  where
    hex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    actual = hexToBase64 hex


runChallenge :: Int -> WriterT [String] IO () -> IO ()
runChallenge nesting chal = do
  msgs <- liftIO strs
  mapM_ putStrLn $ map (indent++) msgs
  where
    strs = execWriterT chal
    indent = take nesting $ repeat ' '

run :: IO ()
run = do
  putStrLn "Set 1:"
  runChallenge 2 challenge1
