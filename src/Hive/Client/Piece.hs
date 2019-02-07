{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Hive.Client.Piece
  ( renderPiece
  ) where

import Data.Foldable (traverse_)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Reflex.Dom.Core

import Hive.Client.Coordinate (SVGCoordinate, asPoints)
import Hive.Client.SVG (svgAttr)
import Hive.Client.SVG.TH (loadSVG)
import Hive.Core.Board (PlayerPiece)
import Hive.Core.Piece (PieceType(..), pieceToType)
import Hive.Core.Player (Player(..))

renderPiece :: MonadWidget t m => SVGCoordinate -> PlayerPiece -> m ()
renderPiece coord (player, piece) = svgAttr "g" attrs $ do
  hexagon
  case pieceToType piece of
    BeeType -> svgBee
    AntType -> svgAnt
    GrasshopperType -> svgGrasshopper
    BeetleType -> svgBeetle
    SpiderType -> svgSpider
  where
    attrs = mconcat
      [ "class" =: Text.unwords
          [ "piece"
          , case player of
              One -> "player1"
              Two -> "player2"
          ]
      , "transform" =: Text.unwords
          [ "scale(0.2)" -- 500x500 -> 100x100
          , "translate(" <> asPoints coord <> ")"
          ]
      ]

{- Helpers -}

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

-- | An SVG hexagon.
hexagon :: MonadWidget t m => m ()
hexagon = svgAttr "polygon" attrs $ pure ()
  where
    attrs = mconcat
      [ "class" =: "hex"
      , "points" =: Text.unwords (map asPoints hexagonPoints)
      ]
    -- regular hexagon, 500px high
    hexagonPoints =
      [ (0            , 250)
      , (250  / sqrt 3, 0  )
      , (750 / sqrt 3 , 0  )
      , (1000 / sqrt 3, 250)
      , (750 / sqrt 3 , 500)
      , (250  / sqrt 3, 500)
      ]
