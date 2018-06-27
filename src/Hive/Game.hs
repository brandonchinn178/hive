module Hive.Game
  ( updateBoard
  ) where

import Hive.Board (Board)
import Hive.Command (Command)
import Hive.Player (Player)

-- | Determine the next state of the board. Error checks the command
-- for invalid commands, such as moving the opponent's piece.
updateBoard :: Board -> Player -> Command -> Either String Board
updateBoard board player command = undefined
