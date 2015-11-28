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
scoreKey bs key = LanguageScore key (englishScore decoded) decoded
  where
    len = fromIntegral $ BS.length bs
    decoded = xorBuffers bs $ BS.pack $ take len $ repeat key


bestScoreWithIndex :: [LanguageScore] -> (Int, LanguageScore)
bestScoreWithIndex = minimumWith (lsScore . snd) . zip [0..]


bestScore :: [LanguageScore] -> LanguageScore
bestScore = snd . bestScoreWithIndex


findOneByteKey :: ByteString -> LanguageScore
findOneByteKey bs = bestScore $ map (scoreKey bs) [0..255]


scoreKeysize :: Fractional a => ByteString -> Int -> a
scoreKeysize buf keySize = total / fromIntegral sampleSize
  where
    sampleSize = 4
    keys = take sampleSize $ groupedBuffer (fromIntegral keySize) buf
    avgDist sample@(k:ks) =
      foldr (\k' s -> s + kDist k k') 0 ks / (fromIntegral $ length sample)
    kDist = curry $ (/ (fromIntegral keySize)) . fromIntegral . uncurry hammingDistance
    distances [_] = []
    distances ks = avgDist ks : distances (tail ks)
    total = sum $ distances keys


findKeySize :: ByteString -> Int
findKeySize bs = fst $ minimumWith snd scores
  where
    sizes = [2..40]
    scores = zip sizes $ map (scoreKeysize bs) sizes


buildKey :: Int -> ByteString -> ByteString
buildKey keySize bs = BS.pack $ map keyByte bufs
  where
    bufs = transposeBuffer (fromIntegral keySize) bs
    keyByte = lsKey . findOneByteKey


findMultibyteKey :: ByteString -> ByteString
findMultibyteKey buf = buildKey (findKeySize buf) buf
