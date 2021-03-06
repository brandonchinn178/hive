{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Hive.Core.Board
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
  , getBorder
  , getCoordinate
  , getNeighborsWithoutNeighbors
  , getOccupied
  , getPiece
  , getPiece'
  , getPosition
  , getReachableSpots
  , getSurrounding
  , getSurroundingPieces
  -- * Board predicates
  , hasNeighbors
  , isHive
  , isOccupied
  , isOnBoard
  ) where

import Control.Monad ((<=<))
import Data.Function (on)
import Data.List (sortBy)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set (Set, (\\))
import qualified Data.Set as Set

import Hive.Core.Coordinate
    (Coordinate, Neighbors(..), getNeighborhood, getNeighbors, toNeighborhood)
import Hive.Core.Piece (Piece, allPieces)
import Hive.Core.Player (Player(..))
import Hive.Core.Utils.Composition ((.:))
import qualified Hive.Core.Utils.Set as Set

{- Auxiliary types -}

-- | A piece on the board and the player that owns the piece.
type PlayerPiece = (Player, Piece)

-- | A coordinate with a value for the height on the board (e.g. beetle stacks),
-- with 0 = bottom.
type Position = (Coordinate, Int)

{- Board type -}

-- | The data type representing the board of a game of Hive.
data Board = Board
  { pieceMap :: Map PlayerPiece (Maybe Position)
    -- ^ Nothing = piece not on board yet. Guaranteed to contain all the pieces
    -- for each player.
  , border   :: Set Coordinate
    -- ^ The empty spots around the board. Guaranteed to be up to date with
    -- pieceMap. Added as a separate field because it's easier to keep track as
    -- pieces are added than recomputing.
  } deriving (Show)

-- | The starting board for a Hive game.
emptyBoard :: Board
emptyBoard = Board
  { pieceMap = Map.fromList
      [((player, piece), Nothing) | player <- [One, Two], piece <- allPieces]
  , border = Set.empty
  }

-- | A map from Coordinate to a list of PlayerPieces, where the head of the list
-- is the top-most piece at that Coordinate.
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
putPiece piece newSpot oldBoard@Board{pieceMap = oldMap, border = oldBorder} =
  newBoard { border = newBorder }
  where
    oldSpot = getCoordinate oldBoard piece
    newBoard = oldBoard
      { pieceMap = Map.insert piece (Just (newSpot, height)) oldMap
      }
    height = maybe 0 ((+ 1) . snd) $ getPiece oldBoard newSpot
    newBorder =
      Set.delete newSpot
      . Set.union (Set.fromMaybe oldSpot)
      . Set.union unoccupiedNeighbors
      . (`Set.difference` prevNeighbors)
      $ oldBorder
    -- all unoccupied neighbors of new position
    unoccupiedNeighbors =
      Set.filter (not . isOccupied oldBoard) $ getNeighborhood newSpot
    -- all neighbors of previous position that don't have any other occupied
    -- neighbors
    prevNeighbors =
      maybe Set.empty (getNeighborsWithoutNeighbors newBoard) oldSpot

-- | Remove the given piece from the board.
--
-- Doesn't occur in an actual game, but useful for figuring out mechanics
-- mid-move.
--
-- Satisfies @removePiece piece . putPiece piece coord = id@
removePiece :: PlayerPiece -> Board -> Board
removePiece piece oldBoard@Board{pieceMap = oldMap, border = oldBorder} =
  newBoard { border = newBorder }
  where
    newBoard = oldBoard { pieceMap = Map.insert piece Nothing oldMap }
    newBorder = case getCoordinate oldBoard piece of
      Nothing -> oldBorder
      Just coordinate ->
        (if hasNeighbors oldBoard coordinate then Set.insert coordinate else id)
        . (`Set.difference` getNeighborsWithoutNeighbors newBoard coordinate)
        $ oldBorder

{- Board queries -}

-- | Get the border of the board.
getBorder :: Board -> Set Coordinate
getBorder = border

-- | Get the coordinate of the given piece.
getCoordinate :: Board -> PlayerPiece -> Maybe Coordinate
getCoordinate = fmap fst .: getPosition

-- | Get the neighbors for a given coordinate that have no neighbors themselves.
--
-- Trivially the empty set if the given coordinate is occupied.
getNeighborsWithoutNeighbors :: Board -> Coordinate -> Set Coordinate
getNeighborsWithoutNeighbors board =
  Set.filter (not . hasNeighbors board) . getNeighborhood

-- | Get all occupied spots on the board.
getOccupied :: Board -> Set Position
getOccupied = Set.fromList . Map.elems . Map.mapMaybe id . pieceMap

-- | Get the top-most piece and its height at the given coordinate.
getPiece :: Board -> Coordinate -> Maybe (PlayerPiece, Int)
getPiece board = getTop <=< (`Map.lookup` coordinateMap board)
  where
    getTop [] = Nothing
    getTop l@(x:_) = Just (x, length l - 1)

-- | Get the top-most piece at the given coordinate.
getPiece' :: Board -> Coordinate -> Maybe PlayerPiece
getPiece' = fmap fst .: getPiece

-- | Get the position of the given piece.
getPosition :: Board -> PlayerPiece -> Maybe Position
getPosition Board{pieceMap} = (pieceMap !)

-- | Get reachable border spots from the given location, optionally filtered
-- by specifying the exact distance from the given coordinate.
--
-- Used for freedom of movement calculations.
getReachableSpots :: Board -> Coordinate -> Maybe Int -> Set Coordinate
getReachableSpots board c distance =
  Map.keysSet
  . Map.filter filterDistance
  $ getReachableSpotsDistance board c
  where
    filterDistance = case distance of
      Nothing -> const True
      Just dist -> (== dist)

-- | Get the pieces in the surrounding coordinates for the given coordinate.
getSurrounding :: Board -> Coordinate -> Neighbors (Maybe PlayerPiece)
getSurrounding board = fmap (getPiece' board) . getNeighbors

-- | Get the surrounding pieces for the given coordinate.
getSurroundingPieces :: Board -> Coordinate -> Set PlayerPiece
getSurroundingPieces = Set.catMaybes . toNeighborhood .: getSurrounding

{- Board predicates -}

-- | Return True if the coordinate has neighbors on the board (regardless of
-- whether the coordinate itself is occupied).
hasNeighbors :: Board -> Coordinate -> Bool
hasNeighbors board = not . Set.null . getSurroundingPieces board

-- | Return True if the board is a contiguous hive.
isHive :: Board -> Bool
isHive board = case fmap fst $ Set.toList $ getOccupied board of
  [] -> True -- nothing is on the board (e.g. the first round)
  (x:xs) -> check [x] $ Set.fromList xs
  where
    check _ (Set.null -> True) = True
    check [] _ = False
    check (x:todo) rest =
      let found = Set.intersection (getNeighborhood x) rest
      in check (todo ++ Set.toList found) $ rest \\ found

-- | Return True if the given coordinate is occupied on the board.
isOccupied :: Board -> Coordinate -> Bool
isOccupied board = isJust . getPiece' board

-- | Return True if the given piece is on the board.
isOnBoard :: Board -> PlayerPiece -> Bool
isOnBoard = isJust .: getPosition

{- Helpers -}

-- | For the given board, get all the reachable spots from a piece at the
-- given coordinate.
--
-- Return a mapping of reachable spots to the number of moves to get to that
-- spot.
getReachableSpotsDistance :: Board -> Coordinate -> Map Coordinate Int
getReachableSpotsDistance board c = search [(c, 0)] Map.empty
  where
    occupied = Set.map fst $ getOccupied board
    search [] m = m
    search ((x, i):xs) m =
      let neighbors = getNeighbors x
          occupiedNeighbors = (`Set.member` occupied) <$> neighbors
          -- check that `target` can move to `dest`, given that the given spots,
          -- if occupied, would prevent the move
          check target (dest, blockers) =
            if dest occupiedNeighbors || all ($ occupiedNeighbors) blockers
              then Nothing
              else Just $ target neighbors
          availableSpots = Set.catMaybes . toNeighborhood $ Neighbors
            { north     = check north     (north, [northwest, northeast])
            , northeast = check northeast (northeast, [north, southeast])
            , southeast = check southeast (southeast, [south, northeast])
            , south     = check south     (south, [southwest, southeast])
            , southwest = check southwest (southwest, [south, northwest])
            , northwest = check northwest (northwest, [north, southwest])
            }
          newSpots = map (, i + 1)
            . Set.toList
            $ availableSpots \\ Map.keysSet m
      in search (xs ++ newSpots) $
          foldl (flip $ uncurry Map.insert) m newSpots
