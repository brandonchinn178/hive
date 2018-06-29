{-# LANGUAGE LambdaCase #-}

module Hive.Command
  ( Command(..)
  , CommandType(..)
  , CommandPiece(..)
  ) where

import Hive.Coordinate (Coordinate)

data Command = Command
  { commandType     :: CommandType
  , commandPiece    :: CommandPiece
  , commandPosition :: Coordinate
  } deriving (Show,Eq)

data CommandType = AddPiece | MovePiece
  deriving (Show,Eq)

data CommandPiece
  = Bee
  | Ant0
  | Ant1
  | Ant2
  | Grass0
  | Grass1
  | Grass2
  | Beetle0
  | Beetle1
  | Spider0
  | Spider1
  deriving (Show,Eq)
