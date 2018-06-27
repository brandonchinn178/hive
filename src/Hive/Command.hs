module Hive.Command
  ( Command(..)
  , parseCommand
  ) where

import Hive.Tile (TileType)

-- | A command that a user can run.
data Command
  = AddPiece TileType

parseCommand :: String -> Maybe Command
parseCommand = undefined
