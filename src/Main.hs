module Main where

import           BufferOps
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as CS
import           DefaultImports
import           Oracle
import qualified Set1
import qualified Set2
import           System.Environment
import           Utils

main :: IO ()
main = do
  putStrLn "\nWelcome to Cryptopals!!!"
  runSet1
  runSet2
  where
    maybeRunSet s run = getArgs >>= return . uncurry (||) . juxt null (elem s) >>= (`when` run)
    runSet1 = maybeRunSet "set1" Set1.run
    runSet2 = maybeRunSet "set2" Set2.run
