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
import qualified Data.Text as Text
import Reflex.Dom.Core

import Hive.Client.SVG.Contrib (svgAttr)
import Hive.Client.SVG.TH (loadSVG)

toSVGPath :: (PostBuild t m, DomBuilder t m) => [String] -> m ()
toSVGPath = svgAttr "g" groupAttrs . traverse_ (fromPath . Text.pack)
  where
    groupAttrs = "transform" =: "scale(0.2)" -- 500x500 -> 100x100
    fromPath d = svgAttr "path" ("d" =: d) $ pure ()

svgAnt :: (PostBuild t m, DomBuilder t m) => m ()
svgAnt = toSVGPath $(loadSVG "ant.svg")

svgBee :: (PostBuild t m, DomBuilder t m) => m ()
svgBee = toSVGPath $(loadSVG "bee.svg")

svgBeetle :: (PostBuild t m, DomBuilder t m) => m ()
svgBeetle = toSVGPath $(loadSVG "beetle.svg")

svgGrasshopper :: (PostBuild t m, DomBuilder t m) => m ()
svgGrasshopper = toSVGPath $(loadSVG "grasshopper.svg")

svgSpider :: (PostBuild t m, DomBuilder t m) => m ()
svgSpider = toSVGPath $(loadSVG "spider.svg")
