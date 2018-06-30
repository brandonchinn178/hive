module Hive.Board
  ( Board
  , PlayerPiece
  , Position
  , emptyBoard
  , isOnBoard
  , getPosition
  , getPiece
  , getSurrounding
  , getSurroundingPieces
  , putPiece
  ) where

import Data.Function (on)
import Data.List (sortBy)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust, mapMaybe)

import Hive.Coordinate (Coordinate, getNeighbors)
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

-- | The starting board for a Hive game.
emptyBoard :: Board
emptyBoard = Board $ Map.fromList
  [ ((player, piece), Nothing)
  | player <- [One, Two]
  , piece <- allPieces
  ]

-- | Returns True if the given piece is on the board.
isOnBoard :: Board -> PlayerPiece -> Bool
isOnBoard board = isJust . getPosition board

-- | Get the board as a map from Coordinate to a list of PlayerPieces, where the head of the
-- list is the top-most piece of the Coordinate.
getFlippedBoard :: Board -> Map Coordinate [PlayerPiece]
getFlippedBoard (Board board) =
  Map.map orderHeight . Map.fromListWith (++) . mapMaybe swap . Map.toList $ board
  where
    swap (_, Nothing) = Nothing
    swap (piece, Just (coord, height)) = Just (coord, [(height, piece)])
    orderHeight = map snd . sortBy (compare `on` fst)

-- | Get the position of the given piece.
getPosition :: Board -> PlayerPiece -> Maybe Position
getPosition (Board board) piece = board ! piece

-- | Get the top-most piece at the given coordinate.
getPiece :: Board -> Coordinate -> Maybe PlayerPiece
getPiece board = fmap head . (`Map.lookup` getFlippedBoard board)

-- | Get the pieces in the surrounding coordinates for the given coordinate.
--
-- The pieces are in the same order as 'getNeighbors'.
getSurrounding :: Board -> Coordinate -> [Maybe PlayerPiece]
getSurrounding board = map (getPiece board) . getNeighbors

-- | Get the surrounding pieces for the given coordinate.
getSurroundingPieces :: Board -> Coordinate -> [PlayerPiece]
getSurroundingPieces board = catMaybes . getSurrounding board

-- | Puts the given piece to the given Position.
putPiece :: Board -> PlayerPiece -> Position -> Board
putPiece (Board board) piece coordinate = Board $ Map.insert piece (Just coordinate) board
