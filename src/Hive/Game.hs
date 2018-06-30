{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Hive.Game
  ( HiveState(..)
  , startState
  , updateState
  ) where

import Control.Monad (unless)
import Data.Bifunctor (second)
import Data.Maybe (isNothing)

import Hive.Board
import Hive.Command
import Hive.Piece
import Hive.Player

-- | The state of a Hive game.
data HiveState = HiveState
  { board     :: Board
  , player    :: Player
  , hiveRound :: Int -- ^ Increments after both players have gone
  }

-- | The starting state for a Hive game.
startState :: HiveState
startState = HiveState emptyBoard One 0

-- | Determine the next state of the board. Error checks the command
-- for invalid commands, such as moving the opponent's piece.
updateState :: HiveState -> Command -> Either String HiveState
updateState HiveState{..} Command{..} = second (const nextState) checkValid
  where
    isPlayerOne = if player == One then True else False
    nextPosition = (commandPosition, 0) -- TODO: if beetle, possibly increment
    nextState = HiveState
      { board = putPiece board (player, commandPiece) nextPosition
      , player = if isPlayerOne then Two else One
      , hiveRound = if isPlayerOne then hiveRound else hiveRound + 1
      }
    noBee = isNothing $ getPosition board (player, Bee)
    checkValid = if hiveRound < 4 && noBee
      then checkStart
      else sequence_ [canLeave, checkValidMovement, canEnter]
    isAdd = isNothing $ getPosition board (player, commandPiece)
    checkStart = do
      -- TODO: no movement allowed
      -- TODO: if hiveRound > 0, can't put next to other color
      undefined
    canLeave = unless isAdd $ do
      -- TODO: check doesn't break hive
      -- TODO: check can slide out of (unless grasshopper or beetle moving up)
      -- TODO: check not under beetle
      undefined
    checkValidMovement = if
      | commandPiece == Bee -> undefined
      | isAnt commandPiece -> undefined
      | isGrasshopper commandPiece -> undefined
      | isBeetle commandPiece -> undefined
      | isSpider commandPiece -> undefined
    canEnter = do
      -- TODO: if isAdd, check only surrounded by same color
      -- TODO: check not occupied (unless beetle)
      -- TODO: check can slide into
      undefined
