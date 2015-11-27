module KeyDetect where

import           BufferOps
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as CS
import           Data.Char
import           Data.List
import qualified Data.Map as M
import           Data.Map.Strict (Map)
import qualified Data.Vector as V
import           Data.Word
import           Types
import           Utils


englishCharacters :: V.Vector Char
englishCharacters = V.fromList [ ' ', 'e',  't', 'a',  'o', 'i', 'n'
                               , 's', 'r',  'h', 'l',  'd', 'c', 'u'
                               , 'm', 'f',  'g', 'p',  'y', 'w', '\n'
                               , 'b', ',',  '.', 'v',  'k', '-', '"'
                               , '_', '\'', 'x', ')',  '(', ';', '0'
                               , 'j', '1',  'q', '=',  '2', ':', 'z'
                               , '/', '*',  '!', '?',  '$', '3', '5'
                               , '>', '{',  '}', '4',  '9', '[', ']'
                               , '8', '6',  '7', '\\', '+', '|', '&'
                               , '<', '%',  '@', '#',  '^', '`', '~'
                               ]


englishScore :: ByteString -> Int
englishScore bs = sumScores orderedChars 0 0
  where
    chars = map toLower $ CS.unpack bs
    freq = frequencies chars
    orderedChars = sortOn (negate . (freq M.!)) (M.keys freq)
    sumScores [] sum _ = sum
    sumScores (r:rs) sum idx =
      case V.elemIndex r englishCharacters of
       Nothing -> sumScores rs (sum + (length englishCharacters - idx)) idx
       Just i -> sumScores rs (sum + abs (idx - i)) (idx + 1)


scoreKey :: ByteString -> Word8 -> LanguageScore
scoreKey bs key = LanguageScore (chr (fromIntegral key)) (englishScore decoded) decoded
  where
    len = fromIntegral $ BS.length bs
    decoded = xorBuffers bs $ BS.pack $ take len $ repeat key


bestScoreWithIndex :: [LanguageScore] -> (Int, LanguageScore)
bestScoreWithIndex = minimumBy compareScores . zip [0..]
  where
    compareScores (_, ls1) (_, ls2) = compare (lsScore ls1) (lsScore ls2)


bestScore :: [LanguageScore] -> LanguageScore
bestScore = snd . bestScoreWithIndex


findOneByteKey :: ByteString -> LanguageScore
findOneByteKey bs = bestScore $ map (scoreKey bs) [0..255]
