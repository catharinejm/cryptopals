module KeyDetect where

import           BufferOps
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as CS
import           Data.Char
import           Data.Int
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
bestScoreWithIndex = minimumWith (lsScore . snd) . zip [0..]


bestScore :: [LanguageScore] -> LanguageScore
bestScore = snd . bestScoreWithIndex


findOneByteKey :: ByteString -> LanguageScore
findOneByteKey bs = bestScore $ map (scoreKey bs) [0..255]


findKeySize :: ByteString -> Int
findKeySize bs = fst $ minimumWith snd scores
  where
    groupsOf n l = BS.take n l : groupsOf n (BS.drop n l)
    score n = let (hd:tl) = groupsOf (fromIntegral n) bs
                  tot = sum $ map (hammingDistance hd) (take 3 tl)
              in (fromIntegral tot) / (fromIntegral n)
    sizes = [2..40]
    scores = zip sizes $ map score sizes
