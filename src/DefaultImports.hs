module DefaultImports
       ( module Prelude
       , module Data.List
       , module DefaultImports
       ) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.List hiding ((++))
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Prelude
import           Prelude hiding ((++))

class Concatenable a where
  (++) :: a -> a -> a

instance Concatenable [a] where
  (++) = (Prelude.++)

instance Concatenable Text where
  a ++ b = T.concat [a, b]

instance Concatenable ByteString where
  a ++ b = BS.concat [a, b]
