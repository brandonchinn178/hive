{-# LANGUAGE LambdaCase #-}

module Hive.Command
  ( Command(..)
  , CommandType(..)
  ) where

import Hive.Coordinate (Coordinate)
import Hive.Piece (Piece)

data Command = Command
  { commandType     :: CommandType
  , commandPiece    :: Piece
  , commandPosition :: Coordinate
  } deriving (Show,Eq)

data CommandType = AddPiece | MovePiece
  deriving (Show,Eq)
