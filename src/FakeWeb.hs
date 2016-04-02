{-# LANGUAGE OverloadedStrings #-}

module FakeWeb where

import           Data.Text (Text)
import qualified Data.Text as T
import           DefaultImports
import           Types

profileFor :: Text -> Text
profileFor email = "email=" ++ saniEmail ++ "&uid=10&role=user"
  where saniEmail = T.filter (not . (`elem` ("&=" :: String))) email

parseQueryString :: Text -> Maybe WebUser
parseQueryString query = WebUser <$> get "email" <*> get "uid" <*> get "role"
  where attrs = map (T.breakOn "=") pairs
        pairs = T.splitOn "&" query
        get k = safeTail . snd <$> find ((== k) . fst) attrs
        safeTail "" = ""
        safeTail t = T.tail t
