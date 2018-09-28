{-# LANGUAGE TemplateHaskell #-}

module Hive.Client.SVG
  ( svgAnt
  , svgBee
  , svgBeetle
  , svgGrasshopper
  , svgSpider
  ) where

import Hive.Client.SVG.TH

-- ** TODO: Convert ParsedSVG to SVG_El

svgAnt :: ParsedSVG
svgAnt = $(loadSVG "ant.svg")

svgBee :: ParsedSVG
svgBee = $(loadSVG "bee.svg")

svgBeetle :: ParsedSVG
svgBeetle = $(loadSVG "beetle.svg")

svgGrasshopper :: ParsedSVG
svgGrasshopper = $(loadSVG "grasshopper.svg")

svgSpider :: ParsedSVG
svgSpider = $(loadSVG "spider.svg")
