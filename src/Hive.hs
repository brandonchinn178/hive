module Hive (playHive) where

import Hive.Board
import Hive.Command
import Hive.Game
import Hive.Player

-- | Play a game of Hive.
playHive :: IO ()
playHive = hiveRound emptyBoard One
  where
    hiveRound board player = do
      putStrLn $ "Current player: " ++ show player
      putStrLn $ show board
      cmd <- getLine
      case parseCommand cmd of
        Nothing -> do
          putStrLn "** Invalid command: could not parse"
          hiveRound board player
        Just cmd' -> case updateBoard board player cmd' of
          Right board' -> hiveRound board' $ succ player
          Left message -> do
            putStrLn $ "** Invalid command: " ++ message
            hiveRound board player
