{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Hive.Client.Board
  ( renderBoard
  ) where

import qualified Data.Map.Strict as Map
import Reflex.Dom.Core

import Hive.Client.Coordinate (toSVGCoord)
import Hive.Client.Piece (renderPiece)
import Hive.Core.Board (Board, coordinateMap)

renderBoard :: MonadWidget t m => Board -> m ()
renderBoard board = mapM_ renderPiece' $ Map.toList $ normalize coordMap
  where
    coordMap = coordinateMap board
    coords = Map.keys coordMap
    renderPiece' (coord, pieces) = case pieces of
      [] -> fail $ "Coordinate has no pieces: " ++ show coord
      piece:_ -> renderPiece (toSVGCoord coord) piece
    -- make Coordinate (0, 0) the top-left corner in the SVG
    leftMost = minimum $ map fst coords
    topMost = maximum $ map snd coords
    normalize = Map.mapKeys $ \(x,y) -> (x + leftMost, y - topMost)
