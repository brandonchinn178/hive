module Hive.Player
  ( Player(..)
  ) where

-- | The data type representing the current player.
data Player = One | Two
  deriving (Show,Eq,Ord)

-- | Mostly for `succ`
instance Enum Player where
  toEnum x = if x `mod` 2 == 0 then One else Two
  fromEnum One = 0
  fromEnum Two = 1
