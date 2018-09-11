{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}

module Hive.Client (app) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import React.Flux

import Hive.Core (Coordinate, HiveState, startState)

data AppState = AppState
  { hiveState  :: HiveState
  , validMoves :: Set Coordinate
  } deriving (Show,Typeable)

data AppAction
  = PutPiece
  | ShowValidMoves
  | ClearValidMoves
  deriving (Show,Typeable)

instance StoreData AppState where
  type StoreAction AppState = AppAction
  transform = undefined

appState :: ReactStore AppState
appState = mkStore $ AppState
  { hiveState = startState
  , validMoves = Set.empty
  }

app :: ReactView ()
app = defineControllerView "Main Hive app" appState $ \state () ->
  p_ $ elemShow state
