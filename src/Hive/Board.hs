module Hive.Board
  ( Board
  , emptyBoard
  , getBoard
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Hive.Coordinate (Coordinate)
import Hive.Piece (Piece, allPieces)
import Hive.Player (Player(..))

-- | The data type representing the board of a game of Hive.
--
-- A board contains all of the pieces for each player, and their
-- coordinates on the board (if they're on the board).
newtype Board = Board (Map (Player, Piece) (Maybe Coordinate))
  deriving (Show)

emptyBoard :: Board
emptyBoard = Board $ Map.fromList
  [ ((player, piece), Nothing)
  | player <- [One, Two]
  , piece <- allPieces
  ]

-- | Define getters so that outside the module, the board cannot be altered.
getBoard :: Board -> Map (Player, Piece) (Maybe Coordinate)
getBoard (Board board) = board
