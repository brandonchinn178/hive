{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Hive.Game
  ( HiveState(..)
  , HiveResult(..)
  , startState
  , getResult
  , updateState
  ) where

import Control.Monad (unless, when)
import Data.Bifunctor (second)
import Data.Maybe (isJust, isNothing)

import Hive.Board
import Hive.Command
import Hive.Coordinate
import Hive.Piece
import Hive.Player

-- | The state of a Hive game.
data HiveState = HiveState
  { board     :: Board
  , player    :: Player
  , hiveRound :: Int -- ^ Increments after both players have gone
  } deriving (Show)

-- | The result of a Hive game.
data HiveResult
  = Win Player -- ^ The given player has won
  | Draw       -- ^ Both queen bees are surrounded
  deriving (Show)

-- | The starting state for a Hive game.
startState :: HiveState
startState = HiveState emptyBoard One 0

-- | Get the result of the Hive game, if it's done.
getResult :: HiveState -> Maybe HiveResult
getResult HiveState{board} = case (isDead One, isDead Two) of
  (False, False) -> Nothing
  (False, True) -> Just $ Win One
  (True, False) -> Just $ Win Two
  (True, True) -> Just Draw
  where
    isDead p = case getPosition board (p, Bee) of
      Nothing -> False
      Just (beeCoordinates, _) -> length (getSurroundingPieces board beeCoordinates) == 6

-- | Determine the next state of the board. Error checks the command
-- for invalid commands, such as moving the opponent's piece.
updateState :: HiveState -> Command -> Either String HiveState
updateState HiveState{..} Command{..} = second (const nextState) checkValid
  where
    otherPlayer = if isPlayerOne then Two else One
    currPosition = getPosition board (player, commandPiece)
    nextSpotPiece = getPiece board commandPosition
    -- Next state
    nextPosition = (commandPosition, 0) -- TODO: if beetle, possibly increment
    nextState = HiveState
      { board = putPiece board (player, commandPiece) nextPosition
      , player = otherPlayer
      , hiveRound = if isPlayerOne then hiveRound else hiveRound + 1
      }
    -- Queries
    isPlayerOne = if player == One then True else False
    noBee = not $ isOnBoard board (player, Bee)
    isAdd = isNothing currPosition
    isNextSpotOccupied = isJust nextSpotPiece
    -- Checks
    checkValid = if hiveRound < 4 && noBee
      then checkStart
      else sequence_ [checkLeave, checkValidMovement, checkEnter]
    checkStart = do
      when (hiveRound == 3 && commandPiece /= Bee) $ Left "Need to place Bee"
      unless isAdd $ Left "Cannot move before placing Bee"
      when (hiveRound > 0) checkWillTouchOtherPlayer
    checkLeave = case currPosition of
      Nothing -> return ()
      Just (coordinate, height) -> do
        unless (height == 0) $ Left "Cannot move a piece currently under a beetle"
        checkCanSlide coordinate
        -- TODO: check doesn't break hive
        undefined
    checkValidMovement = if
      | commandPiece == Bee -> undefined
      | isAnt commandPiece -> undefined
      | isGrasshopper commandPiece -> undefined
      | isBeetle commandPiece -> undefined
      | isSpider commandPiece -> undefined
    checkEnter = do
      -- TODO: if isAdd, check only surrounded by same color
      -- TODO: check not occupied (unless beetle)
      checkCanSlide commandPosition
    checkWillTouchOtherPlayer = when
      (any ((== otherPlayer) . fst) $ getSurroundingPieces board commandPosition)
      $ Left "Cannot add piece next to a piece of the other player"
    checkCanSlide coordinate = unless
      (isGrasshopper commandPiece || (isBeetle commandPiece && isNextSpotOccupied))
      $ do
        let occupied = fmap isJust $ getSurrounding board coordinate
        unless (canSlide occupied) $ Left "Cannot slide in/out of the given position"
