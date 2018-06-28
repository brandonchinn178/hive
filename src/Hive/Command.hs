{-# LANGUAGE LambdaCase #-}

module Hive.Command
  ( Command(..)
  ) where

import Hive.Tile (TileType)

-- | A command a user does on their turn.
data Command
  = AddPiece TileType
