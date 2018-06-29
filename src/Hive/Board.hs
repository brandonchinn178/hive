module Hive.Board
  ( Board(..)
  , Pieces(..)
  , emptyBoard
  ) where

import Hive.Coordinate (Coordinate)

-- | The data type representing the board of a game of Hive.
--
-- A board contains all of the pieces for each player, and their
-- coordinates on the board (if they're on the board).
data Board = Board
  { player1Pieces :: Pieces
  , player2Pieces :: Pieces
  }
  deriving (Show)

emptyBoard :: Board
emptyBoard = Board newPieces newPieces

data Pieces = Pieces
  { bee :: Maybe Coordinate
  , ant0 :: Maybe Coordinate
  , ant1 :: Maybe Coordinate
  , ant2 :: Maybe Coordinate
  , grass0 :: Maybe Coordinate
  , grass1 :: Maybe Coordinate
  , grass2 :: Maybe Coordinate
  , beetle0 :: Maybe Coordinate
  , beetle1 :: Maybe Coordinate
  , spider0 :: Maybe Coordinate
  , spider1 :: Maybe Coordinate
  } deriving (Show)

newPieces :: Pieces
newPieces = Pieces
  { bee = Nothing
  , ant0 = Nothing
  , ant1 = Nothing
  , ant2 = Nothing
  , grass0 = Nothing
  , grass1 = Nothing
  , grass2 = Nothing
  , beetle0 = Nothing
  , beetle1 = Nothing
  , spider0 = Nothing
  , spider1 = Nothing
  }
