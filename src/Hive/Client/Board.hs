{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Hive.Client.Board
  ( renderBoard
  ) where

import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Reflex.Dom.Core

import Hive.Client.Pieces
import Hive.Client.SVG (svgAttr)
import Hive.Core.Board (Board, PlayerPiece, coordinateMap)
import Hive.Core.Coordinate (Coordinate)
import Hive.Core.Piece (PieceType(..), pieceToType)
import Hive.Core.Player (Player(..))

-- | An SVG coordinate corresponding to pieces of height 500px.
type SVGCoordinate = (Double, Double)

renderBoard :: MonadWidget t m => Board -> m ()
renderBoard board = mapM_ (uncurry renderPiece) $ Map.toList $ normalize coordMap
  where
    coordMap = coordinateMap board
    coords = Map.keys coordMap
    -- make Coordinate (0, 0) the top-left corner in the SVG
    leftMost = minimum $ map fst coords
    topMost = maximum $ map snd coords
    normalize = Map.mapKeys $ \(x,y) -> (x + leftMost, y - topMost)

renderPiece :: MonadWidget t m => Coordinate -> [PlayerPiece] -> m ()
renderPiece _ [] = error "Coordinate has no pieces"
renderPiece coord ((player, piece):_) = svgAttr "g" attrs $ do
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
          , "translate(" <> asPoints (toSVGCoord coord) <> ")"
          ]
      ]

{- Helpers -}

-- | Convert a Coordinate to an SVG coordinate.
toSVGCoord :: Coordinate -> SVGCoordinate
toSVGCoord (x, y) = (x' * dx + padding, y' * dyPerY + x' * dyPerX + padding)
  where
    x' = fromIntegral x
    y' = fromIntegral y
    dx = 250 * sqrt 3
    dyPerX = 250
    dyPerY = -500
    padding = 50

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

-- | Render the given coordinate as Text.
asPoints :: SVGCoordinate -> Text
asPoints (x, y) = Text.pack $ show x ++ "," ++ show y
