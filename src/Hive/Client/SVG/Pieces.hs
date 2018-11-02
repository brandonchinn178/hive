{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hive.Client.SVG.Pieces
  ( svgAnt
  , svgBee
  , svgBeetle
  , svgGrasshopper
  , svgSpider
  ) where

import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as Text
import Reflex.Dom.Core

import Hive.Client.SVG.Contrib (svgAttr)
import Hive.Client.SVG.TH (loadSVG)

toSVGPath :: (PostBuild t m, DomBuilder t m) => Text -> [String] -> m ()
toSVGPath cls = svgAttr "g" groupAttrs . traverse_ (fromPath . Text.pack)
  where
    groupAttrs = mconcat
      [ "transform" =: "scale(0.2)" -- 500x500 -> 100x100
      , "class" =: cls
      ]
    fromPath d = svgAttr "path" ("d" =: d) $ pure ()

svgAnt :: (PostBuild t m, DomBuilder t m) => m ()
svgAnt = toSVGPath "ant" $(loadSVG "ant.svg")

svgBee :: (PostBuild t m, DomBuilder t m) => m ()
svgBee = toSVGPath "bee" $(loadSVG "bee.svg")

svgBeetle :: (PostBuild t m, DomBuilder t m) => m ()
svgBeetle = toSVGPath "beetle" $(loadSVG "beetle.svg")

svgGrasshopper :: (PostBuild t m, DomBuilder t m) => m ()
svgGrasshopper = toSVGPath "grasshopper" $(loadSVG "grasshopper.svg")

svgSpider :: (PostBuild t m, DomBuilder t m) => m ()
svgSpider = toSVGPath "spider" $(loadSVG "spider.svg")
