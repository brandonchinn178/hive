{-# LANGUAGE ViewPatterns #-}

module Hive.Board
  ( Board
  , PlayerPiece
  , Position
  , emptyBoard
  , isOnBoard
  , isHiveWithout
  , getBoard
  , getFlippedBoard
  , getPosition
  , getPiece
  , getPiece'
  , getSurrounding
  , getSurroundingPieces
  , putPiece
  ) where

import Control.Monad ((>=>))
import Data.Function (on)
import Data.List (nub, sortBy)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Set ((\\))
import qualified Data.Set as Set

import Hive.Coordinate (Coordinate, Neighbors, getNeighbors, toNeighborhood)
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

-- | Returns True if the board is still a contiguous hive without the given piece.
isHiveWithout :: Board -> PlayerPiece -> Bool
isHiveWithout (Board board) piece = case occupiedSpots of
  [] -> True -- no other piece is on the board (e.g. the first round)
  (x:xs) -> isHive [x] $ Set.fromList xs
  where
    board' = Map.delete piece board
    occupiedSpots = nub . Map.elems . Map.mapMaybe (fmap fst) $ board'
    isHive _ (Set.null -> True) = True
    isHive [] _ = False
    isHive (x:todo) rest =
      let neighbors = Set.fromList $ toNeighborhood $ getNeighbors x
          found = Set.intersection neighbors rest
      in isHive (todo ++ Set.toList found) $ rest \\ found

-- | Get the underlying board.
getBoard :: Board -> Map PlayerPiece (Maybe Position)
getBoard (Board board) = board

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

-- | Get the top-most piece at the given coordinate and its height.
getPiece :: Board -> Coordinate -> Maybe (PlayerPiece, Int)
getPiece board = (`Map.lookup` getFlippedBoard board) >=> getTop
  where
    getTop [] = Nothing
    getTop l@(x:_) = Just (x, length l - 1)

-- | Get the top-most piece at the given coordinate.
getPiece' :: Board -> Coordinate -> Maybe PlayerPiece
getPiece' board = fmap fst . getPiece board

-- | Get the pieces in the surrounding coordinates for the given coordinate.
getSurrounding :: Board -> Coordinate -> Neighbors (Maybe PlayerPiece)
getSurrounding board = fmap (getPiece' board) . getNeighbors

-- | Get the surrounding pieces for the given coordinate.
getSurroundingPieces :: Board -> Coordinate -> [PlayerPiece]
getSurroundingPieces board = catMaybes . toNeighborhood . getSurrounding board

-- | Puts the given piece to the given Position.
putPiece :: Board -> PlayerPiece -> Position -> Board
putPiece (Board board) piece coordinate = Board $ Map.insert piece (Just coordinate) board
