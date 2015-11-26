{-# LANGUAGE ExistentialQuantification #-}

module BufferOps where

import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as CS
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Word
import Data.List
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map as M
import qualified Data.Vector as V

xorBuffers :: ByteString -> ByteString -> ByteString
xorBuffers bs1 bs2 =
  BS.pack $ foldr (\(b1,b2) l -> (b1 `xor` b2) : l) [] $ BS.zip bs1 bs2

frequencies :: String -> Map Char Int
frequencies str = foldr (M.alter inc) M.empty str
  where
    inc Nothing = Just 1
    inc (Just n) = Just $ n+1

englishScore :: BS.ByteString -> Int
englishScore bs = sumScores orderedChars 0 0
  where
    chars = map toLower $ CS.unpack bs
    freq = frequencies chars
    orderedChars = filter (flip elem engLetterOrder) $ sortOn (negate . (freq M.!)) (M.keys freq)
    engLetterOrder = "etaoin shrdlcumwfgypbvkjxqz"
    usedEngLetters = V.fromList $ filter (flip M.member freq) engLetterOrder
    sumScores [] sum _ = sum
    sumScores (r:rs) sum idx =
      case V.elemIndex r usedEngLetters of
       Nothing -> sumScores rs (sum + length engLetterOrder) idx
       Just i -> sumScores rs (sum + abs (idx - i)) (idx + 1)


findOneByteKey :: BS.ByteString -> (Word8, String)
findOneByteKey bs = (key, CS.unpack decoded)
  where
    len = fromIntegral $ BS.length bs
    tryKey b = let key = BS.pack $ take len $ repeat b
                   res = xorBuffers bs key
                   score = englishScore res
               in (score, res)
    (_, key, decoded) = foldr (\b best@(s,_,_) -> let (s',res) = tryKey b
                                                  in if s' < s
                                                     then (s', b, res)
                                                     else best)
                        (0,0,BS.empty) [0..255]
      
    
