module Hive.Core.Command
  ( Command(..)
  ) where

import Hive.Core.Coordinate (Coordinate)
import Hive.Core.Piece (Piece)

-- | A command that moves a given piece to the given coordinate.
data Command = Command
  { commandPiece    :: Piece
  , commandPosition :: Coordinate
  } deriving (Show,Eq)
