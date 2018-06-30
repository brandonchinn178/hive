module Hive.Board
  ( Board
  , PlayerPiece
  , Position
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

-- | A coordinate with a value for the height on the board (e.g. beetle stacks)
type Position = (Coordinate, Int)

-- | The data type representing the board of a game of Hive.
--
-- A board contains all of the pieces for each player, and their
-- position on the board (if they're on the board).
newtype Board = Board (Map PlayerPiece (Maybe Position))
  deriving (Show)

emptyBoard :: Board
emptyBoard = Board $ Map.fromList
  [ ((player, piece), Nothing)
  | player <- [One, Two]
  , piece <- allPieces
  ]

-- | A getter so that the Board data type cannot be altered outside of this module.
getBoard :: Board -> Map PlayerPiece (Maybe Position)
getBoard (Board board) = board

-- | Puts the given piece to the given Position.
putPiece :: Board -> PlayerPiece -> Position -> Board
putPiece (Board board) piece coordinate = Board $ Map.insert piece (Just coordinate) board
