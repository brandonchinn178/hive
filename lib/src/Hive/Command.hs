module Hive.Command
  ( Command(..)
  ) where

import Hive.Coordinate (Coordinate)
import Hive.Piece (Piece)

-- | A command that moves a given piece to the given coordinate.
data Command = Command
  { commandPiece    :: Piece
  , commandPosition :: Coordinate
  } deriving (Show,Eq)
