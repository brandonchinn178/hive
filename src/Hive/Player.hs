module Hive.Player
  ( Player(..)
  ) where

-- | The data type representing the current player.
data Player = One | Two
  deriving (Show,Eq,Ord)
