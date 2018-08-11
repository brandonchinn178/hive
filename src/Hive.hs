{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Hive
  ( module X
  , HiveState(..)
  , HiveResult(..)
  , InvalidCommand(..)
  , startState
  , getResult
  , updateState
  , getValidMoves
  ) where

import Control.Monad (unless, when)
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as Set

import Hive.Board as X
import Hive.Command as X
import Hive.Coordinate as X
import Hive.Piece as X
import Hive.Player as X
import qualified Hive.Utils.Set as Set

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
    isDead p = case getCoordinate board (p, Bee) of
      Nothing -> False
      Just beeCoordinates -> length (getSurroundingPieces board beeCoordinates) == 6

data InvalidCommand
  = NeedToPlaceBee            -- ^ When it's round 4 and the bee has not been placed
  | CannotMoveWithoutBee      -- ^ When the bee isn't on the board yet and player tries to move
  | CannotMoveFromUnderneath  -- ^ When the player tries to move piece under a stack
  | CannotMoveToOccupied      -- ^ When the player tries to move into an occupied spot (not beetle)
  | CannotMoveOffHive         -- ^ When the player tries to move a piece off the hive
  | CannotAddNextToOpponent   -- ^ When the player tries to add a piece next to an opponent's piece
  | ViolatesFreedomOfMovement -- ^ When the player tries to violate "freedom of movement"
  | CannotBreakHive           -- ^ When the player tries to move a piece that would break the hive
  | ViolatesPieceRules        -- ^ When the player tries to move a piece contrary to its type
  deriving (Show)

-- | Determine the next state of the board. Error checks the command
-- for invalid commands, such as moving the opponent's piece.
updateState :: HiveState -> Command -> Either InvalidCommand HiveState
updateState HiveState{..} Command{..} = checkValid >> pure nextState
  where
    otherPlayer = if isPlayerOne then Two else One
    currPiece = (player, commandPiece)
    isCurrOnBoard = isOnBoard board currPiece
    nextSpotPiece = getPiece board commandPosition
    commandPieceType = pieceToType commandPiece
    -- Next state
    nextState = HiveState
      { board = putPiece currPiece commandPosition board
      , player = otherPlayer
      , hiveRound = if isPlayerOne then hiveRound else hiveRound + 1
      }
    -- Queries
    isPlayerOne = if player == One then True else False
    noBee = not $ isOnBoard board (player, Bee)
    isNextSpotOccupied = isJust nextSpotPiece
    -- Checks
    checkValid = if hiveRound < 4 && noBee
      then checkStart
      else sequence_ [checkLeave, checkValidMovement]
    checkStart = do
      when (hiveRound == 3 && commandPiece /= Bee) $ Left NeedToPlaceBee
      unless (not isCurrOnBoard) $ Left CannotMoveWithoutBee
      when (hiveRound > 0) checkWillTouchOtherPlayer
    checkLeave = case getPosition board currPiece of
      Nothing -> return ()
      Just (coordinate, height) -> do
        let topMostHeight = snd $ fromJust $ getPiece board coordinate
        unless (height == topMostHeight) $ Left CannotMoveFromUnderneath
        when (height == 0 && not (isHive $ removePiece currPiece board)) $ Left CannotBreakHive
    checkValidMovement = if isCurrOnBoard
      then do
        unless (commandPieceType == BeetleType || not isNextSpotOccupied)
          $ Left CannotMoveToOccupied
        unless (commandPosition `elem` getBorder (removePiece currPiece board)) $ Left CannotMoveOffHive
        unless (commandPosition `elem` getValidMoves board currPiece) $ Left ViolatesPieceRules
      else checkWillTouchOtherPlayer
    checkWillTouchOtherPlayer = when
      (Set.any ((== otherPlayer) . fst) $ getSurroundingPieces board commandPosition)
      $ Left CannotAddNextToOpponent

-- | Get all the valid moves for the given piece.
--
-- If the piece is not on the board, get the possible positions to put the piece.
getValidMoves :: Board -> PlayerPiece -> Set Coordinate
getValidMoves board playerPiece@(player, _) = case getPosition board playerPiece of
  Nothing -> getValidSpotsOnBorder
  Just pos -> getValidFrom pos
  where
    getValidSpotsOnBorder = Set.filter (not . isTouchingOpponent) $ getBorder (removePiece playerPiece board)
    getValidFrom _ = undefined
    -- Queries
    isTouchingOpponent coord = any ((/= player) . fst) $ getSurroundingPieces board coord
