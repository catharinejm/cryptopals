{-# LANGUAGE OverloadedStrings #-}

module FakeWeb where

import           BufferOps
import           Cipher
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Int
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           DefaultImports
import           Oracle
import           Types

profileFor :: Text -> Text
profileFor email = "email=" ++ saniEmail ++ "&uid=10&role=user"
  where saniEmail = T.filter (not . (`elem` ("&=" :: String))) email

parseQueryString :: Text -> Maybe WebUser
parseQueryString query = WebUser <$> get "email" <*> get "uid" <*> get "role"
  where attrs = map (T.breakOn "=") pairs
        pairs = T.splitOn "&" query
        get k = safeTail . snd <$> find ((== k) . fst) attrs
        safeTail = maybe "" snd . T.uncons

findPad :: AESKey -> Int64
findPad key = foldr catAndEnc 0 [1..]
  where catAndEnc pad cont = if baseLen < (BS.length $ encrypt $ T.replicate pad "x")
                             then pad
                             else cont
        baseLen = BS.length $ encrypt ""
        encrypt = encryptedProfile key

encryptedProfile :: AESKey -> Text -> ByteString
encryptedProfile key = encryptECB key . packText . profileFor

forceAdmin :: AESKey -> Text
forceAdmin key = unpackToText $ decryptECB key (encBaseInit ++ encAdmin)
  where basePad = findPad key + 4
        encrypt = encryptedProfile key
        encryptedBase = encrypt $ T.replicate basePad "x"
        (encBaseInit, encUserTail) = BS.splitAt (BS.length encryptedBase - 16) encryptedBase
        padUserTail = padText "user"
        findMatch n | n >= 16 = error "Failed to find alignment!"
        findMatch n = case matchIdx n of
                       Just i -> (n, i)
                       Nothing -> findMatch (n+1)
        matchIdx n = let encInit = init $ groupedBuffer 16 $ encrypt $ mkAttempt n padUserTail
                     in elemIndex encUserTail encInit
        padText = unpackToText . pkcs7Pad16 . packText
        mkAttempt n txt = T.take (32+basePad) $ T.concat [T.replicate (n+basePad) "x", padText txt, T.repeat 'x']
        (extraPad, block) = findMatch 0
        encAdmin = getBlock (fromIntegral block) $ encrypt $ mkAttempt extraPad $ padText "admin"

packText :: Text -> ByteString
packText = BS.pack . T.unpack

unpackToText :: ByteString -> Text
unpackToText = T.pack . BS.unpack
