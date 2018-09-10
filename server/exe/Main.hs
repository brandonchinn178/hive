{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Network.Wai.Handler.Warp (run)
import Path.IO (resolveDir')
import System.Environment (lookupEnv)

import Hive.Server

main :: IO ()
main = do
  staticDir <- lookupEnv "STATIC_DIR" >>= \case
    Just dir -> resolveDir' dir
    Nothing -> fail "STATIC_DIR is not defined."
  putStrLn "Running on port 3000."
  run 3000 $ app ServerSettings{..}
