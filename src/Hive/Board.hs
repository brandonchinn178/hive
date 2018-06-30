module Hive.Board
  ( Board
  , PlayerPiece
  , emptyBoard
  , getBoard
  , putPiece
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Hive.Coordinate (Coordinate)
import Hive.Piece (Piece, allPieces)
import Hive.Player (Player(..))

type PlayerPiece = (Player, Piece)

-- | The data type representing the board of a game of Hive.
--
-- A board contains all of the pieces for each player, and their
-- coordinates on the board (if they're on the board).
newtype Board = Board (Map PlayerPiece (Maybe Coordinate))
  deriving (Show)

emptyBoard :: Board
emptyBoard = Board $ Map.fromList
  [ ((player, piece), Nothing)
  | player <- [One, Two]
  , piece <- allPieces
  ]

-- | A getter so that the Board data type cannot be altered outside of this module.
getBoard :: Board -> Map PlayerPiece (Maybe Coordinate)
getBoard (Board board) = board

-- | Puts the given piece to the given Coordinate.
putPiece :: Board -> PlayerPiece -> Coordinate -> Board
putPiece (Board board) piece coordinate = Board $ Map.insert piece (Just coordinate) board
