module DefaultImports
       ( module Prelude
       , module Data.List
       , module DefaultImports
       ) where

import           Data.List hiding ((++))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Prelude
import           Prelude hiding ((++))

class Concatenable a where
  (++) :: a -> a -> a

instance Concatenable [a] where
  (++) = (Prelude.++)

instance Concatenable Text where
  a ++ b = T.concat [a, b]
