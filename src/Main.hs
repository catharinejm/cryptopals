module Main where

import qualified Set1
import BufferOps
import qualified Data.ByteString.Lazy.Char8 as CS

main :: IO ()
main = do
  putStrLn "\nWelcome to Cryptopals!!!"
  Set1.run
