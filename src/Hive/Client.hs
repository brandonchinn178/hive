{-# LANGUAGE OverloadedStrings #-}

module Hive.Client (app) where

import Language.Javascript.JSaddle (JSM)
import Reflex.Dom.Core

import Hive.Client.SVG

app :: JSM ()
app = mainWidget $ svgAttr "svg" (mconcat svgAttrs) $ do
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
