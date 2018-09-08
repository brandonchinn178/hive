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
      Just beeCoords -> length (getSurroundingPieces board beeCoords) == 6

data InvalidCommand
  = NeedToPlaceBee            -- ^ When it's round 4 and the bee has not been placed
  | CannotMoveWithoutBee      -- ^ When trying to move before the bee is placed
  | CannotMoveFromUnderneath  -- ^ When trying to move piece under a stack
  | CannotMoveToOccupied      -- ^ When trying to move into an occupied spot (not beetle)
  | CannotMoveOffHive         -- ^ When trying to move a piece off the hive
  | CannotAddNextToOpponent   -- ^ When trying to add a piece next to an opponent's piece
  | ViolatesFreedomOfMovement -- ^ When trying to violate "freedom of movement"
  | CannotBreakHive           -- ^ When trying to move a piece that would break the hive
  | ViolatesPieceRules        -- ^ When trying to move a piece contrary to its type
  deriving (Show)

-- | Check if the given piece can move at all; e.g. not under a stack and
-- doesn't break the hive.
checkCanMove :: Board -> PlayerPiece -> Either InvalidCommand ()
checkCanMove board piece = case getPosition board piece of
  Nothing -> return ()
  Just (coordinate, height) -> do
    let topMostHeight = snd $ fromJust $ getPiece board coordinate
    unless (height == topMostHeight) $ Left CannotMoveFromUnderneath
    when (height == 0 && not (isHive $ removePiece piece board)) $
      Left CannotBreakHive

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
    isPlayerOne = player == One
    noBee = not $ isOnBoard board (player, Bee)
    isNextSpotOccupied = isJust nextSpotPiece
    -- Checks
    errWhen = flip when . Left
    errUnless e = errWhen e . not
    checkValid = if hiveRound < 4 && noBee
      then checkStart
      else sequence_ [checkCanMove board currPiece, checkValidMovement]
    checkStart = do
      errWhen NeedToPlaceBee $ hiveRound == 3 && commandPiece /= Bee
      errUnless CannotMoveWithoutBee $ not isCurrOnBoard
      when (hiveRound > 0) checkWillTouchOtherPlayer
    checkValidMovement = if isCurrOnBoard
      then do
        errUnless CannotMoveToOccupied $
          commandPieceType == BeetleType || not isNextSpotOccupied
        errUnless CannotMoveOffHive $
          commandPosition `elem` getBorder (removePiece currPiece board)
        errUnless ViolatesPieceRules $
          commandPosition `elem` getValidMoves board currPiece
      else checkWillTouchOtherPlayer
    checkWillTouchOtherPlayer = errWhen CannotAddNextToOpponent $
      Set.any ((== otherPlayer) . fst) $
        getSurroundingPieces board commandPosition

-- | Get all the valid moves for the given piece.
--
-- If the piece is not on the board, get the possible positions to put the
-- piece.
getValidMoves :: Board -> PlayerPiece -> Set Coordinate
getValidMoves board playerPiece@(player, piece) =
  case (getPosition board playerPiece, checkCanMove board playerPiece) of
    (Nothing, _) -> Set.filter (not . isTouchingOpponent) borderSpots
    (_, Left _) -> Set.empty
    (Just pos, _) -> getValidFrom pos
  where
    borderSpots = getBorder . removePiece playerPiece $ board
    getValidFrom (coord, _) =
      let getReachableSpots' = getReachableSpots board coord
          occupiedNeighbors =
            Set.filter (isJust . getPiece' board) $ getNeighborhood coord
          availableNeighbors = getReachableSpots' $ Just 1
      in case pieceToType piece of
        BeeType -> availableNeighbors
        AntType -> getReachableSpots' Nothing
        GrasshopperType -> Set.filter (isStraightLine coord) borderSpots
        BeetleType -> Set.union occupiedNeighbors availableNeighbors
        SpiderType -> getReachableSpots' $ Just 3
    -- Queries
    isTouchingOpponent coord =
      any ((/= player) . fst) $ getSurroundingPieces board coord
