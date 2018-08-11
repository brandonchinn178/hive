{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Hive.Board
  ( PlayerPiece
  , Position
  -- * Board type
  , Board
  , emptyBoard
  , coordinateMap
  -- * Board updates
  , putPiece
  , removePiece
  -- * Board queries
  , getNeighborsWithoutNeighbors
  , getPiece
  , getPiece'
  , getSurrounding
  , getSurroundingPieces
  -- * Board predicates
  , hasNeighbors
  , isHive
  , isOccupied
  ) where

import Control.Monad (join, (<=<))
import Data.Function (on)
import Data.List (nub, sortBy)
import Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Set (Set, (\\))
import qualified Data.Set as Set

import Hive.Coordinate (Coordinate, Neighbors, getNeighborhood, getNeighbors, toNeighborhood)
import Hive.Piece (Piece, allPieces)
import Hive.Player (Player(..))

{- Auxiliary types -}

-- | A piece on the board and the player that owns the piece.
type PlayerPiece = (Player, Piece)

-- | A coordinate with a value for the height on the board (e.g. beetle stacks), with 0 = bottom.
type Position = (Coordinate, Int)

{- Board type -}

-- | The data type representing the board of a game of Hive.
data Board = Board
  { pieceMap :: Map PlayerPiece (Maybe Position)
    -- ^ Nothing = piece not on board yet. Guaranteed to contain all the pieces for each player.
  , border :: Set Coordinate
    -- ^ The empty spots around the board. Guaranteed to be up to date with pieceMap. Added as a
    -- separate field because it's easier to keep track as pieces are added than recomputing.
  }

-- | The starting board for a Hive game.
emptyBoard :: Board
emptyBoard = Board
  { pieceMap = Map.fromList
      [((player, piece), Nothing) | player <- [One, Two], piece <- allPieces]
  , border = Set.empty
  }

-- | A map from Coordinate to a list of PlayerPieces, where the head of the list is the top-most
-- piece at that Coordinate.
coordinateMap :: Board -> Map Coordinate [PlayerPiece]
coordinateMap = Map.map orderHeight . invert . Map.mapMaybe id . pieceMap
  where
    invert = fromListCombine . map swap . Map.toList
    swap (piece, (coord, height)) = (coord, (piece, height))
    fromListCombine = Map.fromListWith (++) . map (fmap pure)
    orderHeight = map fst . sortBy (compare `on` snd)

{- Board updates -}

-- | Add or move the given piece to the given coordinate.
putPiece :: PlayerPiece -> Coordinate -> Board -> Board
putPiece piece coordinate oldBoard@Board{pieceMap = oldMap, border = oldBorder} =
  newBoard { border = newBorder }
  where
    newBoard = oldBoard { pieceMap = Map.insert piece (Just (coordinate, height)) oldMap }
    height = maybe 0 ((+ 1) . snd) $ getPiece oldBoard coordinate
    newBorder =
      Set.delete coordinate
      . Set.union unoccupiedNeighbors
      . (`Set.difference` prevNeighbors)
      $ oldBorder
    -- all unoccupied neighbors of new position
    unoccupiedNeighbors = Set.filter (not . isOccupied oldBoard) $ getNeighborhood coordinate
    -- all neighbors of previous position that don't have any other occupied neighbors
    prevNeighbors = case oldMap ! piece of
      Nothing -> Set.empty
      Just (prev, _) -> getNeighborsWithoutNeighbors newBoard prev

-- | Remove the given piece from the board.
--
-- Doesn't occur in an actual game, but useful for figuring out mechanics mid-move.
removePiece :: PlayerPiece -> Board -> Board
removePiece piece oldBoard@Board{pieceMap = oldMap, border = oldBorder} = board
  { pieceMap = newMap
  , border = newBorder
  }
  where
    newBoard = board { pieceMap = Map.insert piece Nothing oldMap }
    coordinate = fst $ oldMap ! piece
    newBorder =
      Set.add coordinate
      . (`Set.difference` getNeighborsWithoutNeighbors newBoard coordinate)
      $ oldBorder

{- Board queries -}

-- | Get the neighbors for a given coordinate that have no neighbors themselves.
--
-- Trivially the empty set if the given coordinate is occupied.
getNeighborsWithoutNeighbors :: Board -> Coordinate -> Set Coordinate
getNeighborsWithoutNeighbors board = Set.filter (not . hasNeighbors board) . getNeighborhood

-- | Get the top-most piece and its height at the given coordinate.
getPiece :: Board -> Coordinate -> Maybe (PlayerPiece, Int)
getPiece board = getTop <=< (coordinateMap board !?)
  where
    getTop [] = Nothing
    getTop l@(x:_) = Just (x, length l - 1)

-- | Get the top-most piece at the given coordinate.
getPiece' :: Board -> Coordinate -> Maybe PlayerPiece
getPiece' = fmap fst .: getPiece

-- | Get the pieces in the surrounding coordinates for the given coordinate.
getSurrounding :: Board -> Coordinate -> Neighbors (Maybe PlayerPiece)
getSurrounding board = fmap (getPiece' board) . getNeighbors

-- | Get the surrounding pieces for the given coordinate.
getSurroundingPieces :: Board -> Coordinate -> Set PlayerPiece
getSurroundingPieces = catMaybesSet . toNeighborhood .: getSurrounding

{- Board predicates -}

-- | Return True if the coordinate has neighbors on the board (regardless of whether the coordinate
-- itself is occupied).
hasNeighbors :: Board -> Coordinate -> Bool
hasNeighbors board = not . Set.null . getSurroundingPieces board

-- | Return True if the board is a contiguous hive.
isHive :: Board -> Bool
isHive Board{pieceMap} = case occupiedSpots of
  [] -> True -- nothing is on the board (e.g. the first round)
  (x:xs) -> check [x] $ Set.fromList xs
  where
    occupiedSpots = Map.elems . Map.mapMaybe (fmap fst) $ pieceMap
    check _ (Set.null -> True) = True
    check [] _ = False
    check (x:todo) rest =
      let found = Set.intersection (getNeighborhood x) rest
      in check (todo ++ Set.toList found) $ rest \\ found

-- | Return True if the given coordinate is occupied on the board.
isOccupied :: Board -> Coordinate -> Bool
isOccupied board = isJust . getPiece' board

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
      let found = Set.intersection (getNeighbors' x) rest
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
getPosition (toBoard -> board) piece = join $ Map.lookup piece board

-- | Get the top-most piece at the given coordinate and its height.
getPiece :: BoardLike b => b -> Coordinate -> Maybe (PlayerPiece, Int)
getPiece board = (`Map.lookup` getFlippedBoard board) >=> getTop
  where
    getTop [] = Nothing
    getTop l@(x:_) = Just (x, length l - 1)

-- | Get the top-most piece at the given coordinate.
getPiece' :: BoardLike b => b -> Coordinate -> Maybe PlayerPiece
getPiece' = fmap fst .: getPiece

-- | Get the coordinates around the hive without the given piece.
getBorderWithout :: BoardLike b => b -> PlayerPiece -> [Coordinate]
getBorderWithout (toBoard -> board) piece =
  filter (`notElem` occupiedSpots) . getAllNeighbors $ occupiedSpots
  where
    occupiedSpots = getOccupiedSpots (board `without` piece)
    getAllNeighbors = nub . concatMap getNeighbors'

{- Helpers -}

-- | 'catMaybes' for Set
catMaybesSet :: Set (Maybe a) -> Set a
catMaybesSet = Set.map fromJust . Set.filter isJust

(.:) :: (z -> c) -> (a -> b -> z) -> a -> b -> c
(.:) = (.) . (.)
infixl 4 .: -- same fixity as <$>
