module Hive.Piece
  ( Piece(..)
  , allPieces
  ) where

data Piece
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
  deriving (Show,Eq,Ord)

allPieces :: [Piece]
allPieces =
  [ Bee
  , Ant0
  , Ant1
  , Ant2
  , Grass0
  , Grass1
  , Grass2
  , Beetle0
  , Beetle1
  , Spider0
  , Spider1
  ]
