{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Hive.Game
  ( HiveState(..)
  , HiveResult(..)
  , InvalidCommand(..)
  , startState
  , getResult
  , updateState
  ) where

import Control.Monad (unless, when)
import Data.Bifunctor (second)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)

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

data InvalidCommand
  = NeedToPlaceBee           -- ^ When it's round 4 and the bee has not been placed
  | CannotMoveWithoutBee     -- ^ When the bee isn't on the board yet and player tries to move
  | CannotMoveFromUnderneath -- ^ When the player tries to move piece under a stack
  | CannotMoveToOccupied     -- ^ When the player tries to move into an occupied spot (not beetle)
  | CannotAddNextToOpponent  -- ^ When the player tries to add a piece next to an opponent's piece
  | UnableToSlide            -- ^ When the player tries to violate "freedom of movement"
  | CannotBreakHive          -- ^ When the player tries to move a piece that would break the hive
  deriving (Show)

-- | Determine the next state of the board. Error checks the command
-- for invalid commands, such as moving the opponent's piece.
updateState :: HiveState -> Command -> Either InvalidCommand HiveState
updateState HiveState{..} Command{..} = second (const nextState) checkValid
  where
    otherPlayer = if isPlayerOne then Two else One
    currPiece = (player, commandPiece)
    currPosition = getPosition board currPiece
    nextSpotPiece = getPiece board commandPosition
    commandPieceType = pieceToType commandPiece
    -- Next state
    nextHeight = fromMaybe 0 $ ((+ 1) . snd) <$> nextSpotPiece
    nextPosition = (commandPosition, nextHeight)
    nextState = HiveState
      { board = putPiece board currPiece nextPosition
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
      when (hiveRound == 3 && commandPiece /= Bee) $ Left NeedToPlaceBee
      unless isAdd $ Left CannotMoveWithoutBee
      when (hiveRound > 0) checkWillTouchOtherPlayer
    checkLeave = case currPosition of
      Nothing -> return ()
      Just (coordinate, height) -> do
        let topMostHeight = snd $ fromJust $ getPiece board coordinate
        unless (height == topMostHeight) $ Left CannotMoveFromUnderneath
        checkCanSlide coordinate
        when (height == 0 && not (isHiveWithout board currPiece)) $ Left CannotBreakHive
    checkValidMovement = case commandPieceType of
      BeeType -> undefined
      AntType -> undefined
      GrasshopperType -> undefined
      BeetleType -> undefined
      SpiderType -> undefined
    checkEnter = do
      when isAdd checkWillTouchOtherPlayer
      unless (commandPieceType == BeetleType || not isNextSpotOccupied) $ Left CannotMoveToOccupied
      checkCanSlide commandPosition
    checkWillTouchOtherPlayer = when
      (any ((== otherPlayer) . fst) $ getSurroundingPieces board commandPosition)
      $ Left CannotAddNextToOpponent
    checkCanSlide coordinate =
      let isSlide = not . or $
            [ commandPieceType == GrasshopperType
            , commandPieceType == BeetleType && isNextSpotOccupied
            ]
          occupied = isJust <$> getSurrounding board coordinate
      in unless (not isSlide || canSlide occupied) $ Left UnableToSlide
