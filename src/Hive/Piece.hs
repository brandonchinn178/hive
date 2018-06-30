{-# LANGUAGE LambdaCase #-}

module Hive.Piece
  ( Piece(..)
  , PieceType(..)
  , allPieces
  , pieceToType
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

data PieceType = BeeType | AntType | GrasshopperType | BeetleType | SpiderType
  deriving (Show,Eq)

pieceToType :: Piece -> PieceType
pieceToType = \case
  Bee -> BeeType
  Ant0 -> AntType
  Ant1 -> AntType
  Ant2 -> AntType
  Grass0 -> GrasshopperType
  Grass1 -> GrasshopperType
  Grass2 -> GrasshopperType
  Beetle0 -> BeetleType
  Beetle1 -> BeetleType
  Spider0 -> SpiderType
  Spider1 -> SpiderType
