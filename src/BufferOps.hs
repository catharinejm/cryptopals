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
englishScore bs = foldl' (\s l -> s + score l) 0 chars
  where
    chars = map toLower $ CS.unpack bs
    freq = frequencies chars
    orderedChars = V.fromList $ filter (flip elem engLetterOrder) $ sortOn (negate . (freq M.!)) (M.keys freq)
    engLetterOrder = "etaoin shrdlu"
    usedEngLetters = V.fromList $ filter (flip M.member freq) engLetterOrder
    score l = let idx = V.elemIndex l
              in case fmap (-) (idx orderedChars) <*> (idx usedEngLetters) of
                  Nothing -> 0
                  Just s -> length usedEngLetters - abs s


findOneByteKey :: BS.ByteString -> (Word8, String)
findOneByteKey bs = (key, CS.unpack decoded)
  where
    len = fromIntegral $ BS.length bs
    tryKey b = let key = BS.pack $ take len $ repeat b
                   res = xorBuffers bs key
                   score = englishScore res
               in (score, res)
    (_, key, decoded) = foldr (\b best@(s,_,_) -> let (s',res) = tryKey b
                                                  in if s' > s
                                                     then (s', b, res)
                                                     else best)
                        (0,0,BS.empty) [0..255]
      
    
