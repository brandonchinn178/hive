module Hive.Tile
  ( Tile(..)
  , TileType(..)
  ) where

import Hive.Player (Player)

-- | A tile in the board.
data Tile = Tile
  { owner :: Player
  , kind :: TileType
  }

-- | The type of each tile on the board.
data TileType
  = Bee
  | Ant
  | Grasshopper
  | Beetle
  | Spider
  deriving (Show,Eq)
