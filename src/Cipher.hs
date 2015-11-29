module Cipher where

import qualified Crypto.Cipher.AES as AES
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as BS

initAES :: BS.ByteString -> AES.AES
initAES = AES.initAES . BS.toStrict

decryptECB :: AES.AES -> BS.ByteString -> BS.ByteString
decryptECB key = BS.fromStrict . AES.decryptECB key . BS.toStrict
