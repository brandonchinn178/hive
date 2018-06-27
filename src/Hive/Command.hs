{-# LANGUAGE LambdaCase #-}

module Hive.Command
  ( Command(..)
  , HiveCommand(..)
  , ShowTarget(..)
  , getCommand
  ) where

import Data.List (isPrefixOf)

import Hive.Tile (TileType)

-- | A command parsed from user input.
data Command
  = Hive HiveCommand
  | Show ShowTarget
  | Help

-- | A Hive command.
data HiveCommand
  = AddPiece TileType

-- | A target for 
data ShowTarget = ShowBoard | ShowPlayer

-- | Get a command from user input.
getCommand :: IO Command
getCommand = loop
  where
    getInput = putStr "hive> " >> getLine
    loop = getInput >>= \case
      "help" -> printHelp >> loop
      s | "show " `isPrefixOf` s -> case drop 5 s of
        "board" -> return $ Show ShowBoard
        "player" -> return $ Show ShowPlayer
        _ -> loopError "Invalid show target."
      _ -> loopError "Could not parse command."
    loopError message = do
      putStrLn $ "** " ++ message
      putStrLn $ "** See 'help' for more details."
      loop

-- | Print out the help text.
printHelp :: IO ()
printHelp = mapM_ putStrLn
  [ "Available commands:"
  , "show [TARGET]      Show the given target."
  , "                   Available targets: 'board', 'player'"
  , "help               Print this help text"
  ]
