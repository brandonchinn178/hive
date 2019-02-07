module Hive.Client.Coordinate
  ( SVGCoordinate
  , toSVGCoord
  , asPoints
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Hive.Core.Coordinate (Coordinate)

-- | An SVG coordinate corresponding to pieces of height 500px.
type SVGCoordinate = (Double, Double)

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

-- | Render the given coordinate as Text.
asPoints :: SVGCoordinate -> Text
asPoints (x, y) = Text.pack $ show x ++ "," ++ show y
