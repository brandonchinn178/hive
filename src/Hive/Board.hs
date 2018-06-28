module Hive.Board
  ( Board
  , emptyBoard
  ) where

import Hive.Tile (Tile)

-- | The data type representing the board of a game of Hive.
newtype Board = Board [Tile]
  deriving (Show)

instance Show Board where
  show (Board tiles) = undefined

emptyBoard :: Board
emptyBoard = Board []
