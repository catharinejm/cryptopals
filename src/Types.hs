module Types where

import Data.ByteString.Lazy (ByteString)

data LanguageScore = LanguageScore { lsKey     :: !Char
                                   , lsScore   :: !Int
                                   , lsDecoded :: !ByteString
                                   }
                   deriving (Show)
