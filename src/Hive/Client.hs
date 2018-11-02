{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hive.Client (app) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (lift, runIO)
import Language.Javascript.JSaddle (JSM)
import Reflex.Dom.Core
import System.Process (readProcess)

import Hive.Client.SVG

app :: JSM ()
app = mainWidgetWithCss style $ svgAttr "svg" (mconcat svgAttrs) $ do
    svgAnt
    svgBee
    svgBeetle
    svgGrasshopper
    svgSpider
  where
    svgAttrs =
      [ "viewBox" =: "0 0 500 500"
      , "width" =: "500"
      , "height" =: "500"
      ]

style :: ByteString
style = $(do
  result <- runIO $ readProcess "sass" ["static/style.scss"] ""
  lift $ pack result
  )
