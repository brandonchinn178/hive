module Hive.Piece
  ( Piece(..)
  , allPieces
  , isAnt
  , isGrasshopper
  , isBeetle
  , isSpider
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

isAnt, isGrasshopper, isBeetle, isSpider :: Piece -> Bool
isAnt = (`elem` [Ant0, Ant1, Ant2])
isGrasshopper = (`elem` [Grass0, Grass1, Grass2])
isBeetle = (`elem` [Beetle0, Beetle1])
isSpider = (`elem` [Spider0, Spider1])
