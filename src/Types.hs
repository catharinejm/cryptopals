module Types where

import Data.ByteString.Lazy (ByteString)

data LanguageScore = LanguageScore { lsKey     :: !Char
                                   , lsScore   :: !Int
                                   , lsDecoded :: !ByteString
                                   }
                   deriving (Show)

compareScores :: LanguageScore -> LanguageScore -> Ordering
compareScores ls1 ls2 = compare (lsScore ls1) (lsScore ls2)
