{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Hive.Board
  ( Board
  , HiveBoard
  , BoardLike(..)
  , PlayerPiece
  , Position
  , emptyBoard
  , isOnBoard
  , isHiveWithout
  , putPiece
  , getFlippedBoard
  , getOccupiedSpots
  , getPosition
  , getPiece
  , getPiece'
  , getSurrounding
  , getSurroundingPieces
  , getBorderWithout
  ) where

import Control.Monad ((>=>))
import Data.Function (on)
import Data.List (nub, sortBy)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Set ((\\))
import qualified Data.Set as Set

import Hive.Coordinate (Coordinate, Neighbors, getNeighbors, getNeighbors', toNeighborhood)
import Hive.Piece (Piece, allPieces)
import Hive.Player (Player(..))

{- Types and constructors -}

-- | A piece on the board and the player that owns the piece.
type PlayerPiece = (Player, Piece)

-- | A coordinate with a value for the height on the board (e.g. beetle stacks)
type Position = (Coordinate, Int)

-- | A board of pieces and their positions. May or may not have all the pieces.
type Board = Map PlayerPiece (Maybe Position)

-- | The data type representing the board of a game of Hive.
--
-- Guaranteed to contain all the pieces for each player.
newtype HiveBoard = HiveBoard Board
  deriving (Show)

-- | A type class generalizing any Board data type.
class BoardLike b where
  toBoard :: b -> Board

instance BoardLike Board where
  toBoard = id

instance BoardLike HiveBoard where
  toBoard (HiveBoard board) = board

-- | The starting board for a Hive game.
emptyBoard :: HiveBoard
emptyBoard = HiveBoard $ Map.fromList
  [ ((player, piece), Nothing)
  | player <- [One, Two]
  , piece <- allPieces
  ]

{- Functions -}

-- | Returns a board without the given piece.
without :: BoardLike b => b -> PlayerPiece -> Board
without = flip Map.delete . toBoard

-- | Returns True if the given piece is on the board.
isOnBoard :: BoardLike b => b -> PlayerPiece -> Bool
isOnBoard = isJust .: getPosition

-- | Returns True if the board is still a contiguous hive without the given piece.
isHiveWithout :: BoardLike b => b -> PlayerPiece -> Bool
isHiveWithout (toBoard -> board) piece = case getOccupiedSpots (board `without` piece) of
  [] -> True -- no other piece is on the board (e.g. the first round)
  (x:xs) -> isHive [x] $ Set.fromList xs
  where
    isHive _ (Set.null -> True) = True
    isHive [] _ = False
    isHive (x:todo) rest =
      let neighbors = Set.fromList $ getNeighbors' x
          found = Set.intersection neighbors rest
      in isHive (todo ++ Set.toList found) $ rest \\ found

-- | Puts the given piece to the given Position.
putPiece :: HiveBoard -> PlayerPiece -> Position -> HiveBoard
putPiece (HiveBoard board) piece coordinate = HiveBoard $ Map.insert piece (Just coordinate) board

-- | Get the board as a map from Coordinate to a list of PlayerPieces, where the head of the
-- list is the top-most piece of the Coordinate.
getFlippedBoard :: BoardLike b => b -> Map Coordinate [PlayerPiece]
getFlippedBoard = Map.map orderHeight . Map.fromListWith (++) . mapMaybe swap . Map.toList . toBoard
  where
    swap (_, Nothing) = Nothing
    swap (piece, Just (coord, height)) = Just (coord, [(height, piece)])
    orderHeight = map snd . sortBy (compare `on` fst)

-- | Get all occupied coordinates on the board.
getOccupiedSpots :: BoardLike b => b -> [Coordinate]
getOccupiedSpots = nub . Map.elems . Map.mapMaybe (fmap fst) . toBoard

-- | Get the position of the given piece.
getPosition :: BoardLike b => b -> PlayerPiece -> Maybe Position
getPosition (toBoard -> board) piece = board ! piece

-- | Get the top-most piece at the given coordinate and its height.
getPiece :: BoardLike b => b -> Coordinate -> Maybe (PlayerPiece, Int)
getPiece board = (`Map.lookup` getFlippedBoard board) >=> getTop
  where
    getTop [] = Nothing
    getTop l@(x:_) = Just (x, length l - 1)

-- | Get the top-most piece at the given coordinate.
getPiece' :: BoardLike b => b -> Coordinate -> Maybe PlayerPiece
getPiece' = fmap fst .: getPiece

-- | Get the pieces in the surrounding coordinates for the given coordinate.
getSurrounding :: BoardLike b => b -> Coordinate -> Neighbors (Maybe PlayerPiece)
getSurrounding board = fmap (getPiece' board) . getNeighbors

-- | Get the surrounding pieces for the given coordinate.
getSurroundingPieces :: BoardLike b => b -> Coordinate -> [PlayerPiece]
getSurroundingPieces = catMaybes . toNeighborhood .: getSurrounding

-- | Get the coordinates around the hive without the given piece.
getBorderWithout :: BoardLike b => b -> PlayerPiece -> [Coordinate]
getBorderWithout (toBoard -> board) piece =
  filter (`notElem` occupiedSpots) . getAllNeighbors $ occupiedSpots
  where
    occupiedSpots = getOccupiedSpots (board `without` piece)
    getAllNeighbors = nub . concatMap getNeighbors'

{- Helpers -}

(.:) :: (z -> c) -> (a -> b -> z) -> a -> b -> c
(.:) = (.) . (.)
infixl 4 .: -- same fixity as <$>
