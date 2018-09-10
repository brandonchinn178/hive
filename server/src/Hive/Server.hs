{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Hive.Server (ServerSettings(..), app) where

import qualified Data.ByteString.Lazy as ByteString
import Data.FileEmbed (embedFile)
import Data.Proxy (Proxy)
import Network.Wai (Application)
import Path (Abs, Dir, Path, fromAbsDir)
import Servant
import Servant.Utils.StaticFiles (serveDirectoryFileServer)

import Hive.Server.ContentTypes (HTML, RawHtml(..))

data ServerSettings = ServerSettings
  { staticDir :: Path Abs Dir
  }

type IndexPage = Get '[HTML] RawHtml

getIndexPage :: Handler RawHtml
getIndexPage = return . RawHtml . ByteString.fromStrict $ indexPage
  where
    indexPage = $(embedFile "static/index.html")

type StaticFiles = "static" :> Raw

getStaticFiles :: ServerSettings -> Server Raw
getStaticFiles ServerSettings{staticDir} =
  serveDirectoryFileServer $ fromAbsDir staticDir

type HiveApi = IndexPage :<|> StaticFiles

hiveApi :: ServerSettings -> Server HiveApi
hiveApi settings = getIndexPage :<|> getStaticFiles settings

app :: ServerSettings -> Application
app = serve (Proxy :: Proxy HiveApi) . hiveApi
