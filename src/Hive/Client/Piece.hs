{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Hive.Client.Piece
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

import Hive.Client.SVG (svgAttr)
import Hive.Client.SVG.TH (loadSVG)

toSVGPath :: MonadWidget t m => Text -> [String] -> m ()
toSVGPath cls = svgAttr "g" attrs . traverse_ (fromPath . Text.pack)
  where
    attrs = mconcat
      [ "class" =: cls
      , "transform" =: "translate(35)" -- manually bump pieces to center
      ]
    fromPath d = svgAttr "path" ("d" =: d) $ pure ()

svgAnt :: MonadWidget t m => m ()
svgAnt = toSVGPath "ant" $(loadSVG "ant.svg")

svgBee :: MonadWidget t m => m ()
svgBee = toSVGPath "bee" $(loadSVG "bee.svg")

svgBeetle :: MonadWidget t m => m ()
svgBeetle = toSVGPath "beetle" $(loadSVG "beetle.svg")

svgGrasshopper :: MonadWidget t m => m ()
svgGrasshopper = toSVGPath "grasshopper" $(loadSVG "grasshopper.svg")

svgSpider :: MonadWidget t m => m ()
svgSpider = toSVGPath "spider" $(loadSVG "spider.svg")
