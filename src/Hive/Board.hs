{-# LANGUAGE TupleSections #-}

module Hive.Board
  ( Board
  , emptyBoard
  , player1Pieces
  , player2Pieces
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Hive.Coordinate (Coordinate)
import Hive.Piece (Piece, allPieces)

-- | The data type representing the board of a game of Hive.
--
-- A board contains all of the pieces for each player, and their
-- coordinates on the board (if they're on the board).
data Board = Board
  { _player1 :: Map Piece (Maybe Coordinate)
  , _player2 :: Map Piece (Maybe Coordinate)
  }
  deriving (Show)

emptyBoard :: Board
emptyBoard = Board newPieces newPieces
  where
    newPieces = Map.fromList $ map (, Nothing) allPieces

-- | Define getters so that outside the module, the board cannot be altered.
player1Pieces, player2Pieces :: Board -> Map Piece (Maybe Coordinate)
player1Pieces = _player1
player2Pieces = _player2
