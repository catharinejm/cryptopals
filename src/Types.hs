{-# LANGUAGE NamedFieldPuns #-}

module Types where

import qualified Crypto.Cipher.AES as AES
import           Data.ByteString.Lazy (ByteString)
import           Data.Word

data Challenge = Challenge { chalNum      :: !Int
                           , chalExpected :: !String
                           , chalActual   :: !String
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


data LanguageScore = LanguageScore { lsKey     :: !Word8
                                   , lsScore   :: !Int
                                   , lsDecoded :: !ByteString
                                   }
                   deriving (Show)

type AESKey = AES.AES
