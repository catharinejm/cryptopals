module Main where

import           BufferOps
import qualified Data.ByteString.Lazy.Char8 as CS
import           Utils
import qualified Set1

main :: IO ()
main = do
  putStrLn "\nWelcome to Cryptopals!!!"
  Set1.run
