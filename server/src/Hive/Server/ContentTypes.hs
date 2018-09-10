{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Hive.Server.ContentTypes
  ( HTML
  , RawHtml(..)
  ) where

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Media ((//), (/:))
import Servant.API.ContentTypes (Accept(..), MimeRender(..))

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

newtype RawHtml = RawHtml ByteString

instance MimeRender HTML RawHtml where
  mimeRender _ (RawHtml bs) = bs
