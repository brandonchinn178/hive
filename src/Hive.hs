{-# LANGUAGE LambdaCase #-}

module Hive (playHive) where

import Hive.Board (emptyBoard)
import Hive.Command (Command(..), getCommand)
import Hive.Game (updateBoard)
import Hive.Player (Player(..))

-- | Play a game of Hive.
playHive :: IO ()
playHive = do
  putStrLn "Starting a game of Hive...\n"
  hiveRound emptyBoard One
  where
    hiveRound board player = do
      putStrLn $ "Current player: " ++ show player
      putStrLn $ show board
      getCommand >>= \case
        Help -> undefined
        Show target -> undefined
        Hive cmd -> case updateBoard board player cmd of
          Right board' -> hiveRound board' $ succ player
          Left message -> do
            putStrLn $ "** Invalid command: " ++ message
            hiveRound board player
