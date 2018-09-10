{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Hive.Server (app) where

import qualified Data.ByteString.Lazy as ByteString
import Data.FileEmbed (embedFile)
import Data.Proxy (Proxy)
import Network.Wai (Application)
import Servant
import Servant.Utils.StaticFiles (serveDirectoryFileServer)

import Hive.Server.ContentTypes (HTML, RawHtml(..))

type IndexPage = Get '[HTML] RawHtml

getIndexPage :: Handler RawHtml
getIndexPage = return . RawHtml . ByteString.fromStrict $ indexPage
  where
    indexPage = $(embedFile "static/index.html")

type StaticFiles = "static" :> Raw

getStaticFiles :: Server Raw
getStaticFiles = serveDirectoryFileServer "server/static"

type HiveApi = IndexPage :<|> StaticFiles

hiveApi :: Server HiveApi
hiveApi = getIndexPage :<|> getStaticFiles

app :: Application
app = serve (Proxy :: Proxy HiveApi) hiveApi
