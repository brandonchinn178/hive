module Hive.Game
  ( HiveState(..)
  , updateState
  ) where

import Hive.Board (Board)
import Hive.Command (Command)
import Hive.Player (Player)

-- | The state of a Hive game.
data HiveState = HiveState
  { board :: Board
  , player :: Player
  }

-- | Determine the next state of the board. Error checks the command
-- for invalid commands, such as moving the opponent's piece.
updateState :: HiveState -> Command -> Either String Board
updateState = undefined
