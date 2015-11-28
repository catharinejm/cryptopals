module Types where

import Data.ByteString.Lazy (ByteString)
import Data.Word

data LanguageScore = LanguageScore { lsKey     :: !Word8
                                   , lsScore   :: !Int
                                   , lsDecoded :: !ByteString
                                   }
                   deriving (Show)
