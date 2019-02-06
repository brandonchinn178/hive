{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hive.Client (app) where

import Control.Monad (zipWithM)
import qualified Data.Text as Text
import Hasmin (minifyCSS)
import Language.Haskell.TH.Syntax (addDependentFile, lift, runIO)
import Language.Javascript.JSaddle (JSM)
import Reflex.Dom.Core
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.Process (readProcess)

import Hive.Client.Board (renderBoard)
import Hive.Client.Coordinate (toSVGCoord)
import Hive.Client.Piece (renderPiece)
import Hive.Client.SVG (svg', svgAttr)
import Hive.Core.Board (emptyBoard, putPiece)
import Hive.Core.Piece (Piece(..), PieceType(..))
import Hive.Core.Player (Player(..))

app :: JSM ()
app = mainWidgetWithHead header body

header :: MonadWidget t m => m ()
header = do
  el "title" $ text "Hive"
  case minifyCSS $ Text.pack style of
    Right css -> el "style" $ text css
    _ -> fail "Bad style.scss file"

body :: MonadWidget t m => m ()
body = svgAttr "svg" ("viewBox" =: "0 0 1000 1000") $
  -- TODO: make header with title and game info
  if True -- TODO: make game loop
    then do
      -- TODO: show text for player that is currently selecting
      _ <- startBox One
      -- TODO: take first click as the player's choice
      return ()
    else
      -- TODO: render the board at the current state
      renderBoard
        $ putPiece (One, Bee) (0, -1)
        . putPiece (Two, Grass0) (1, 2)
        . putPiece (One, Beetle0) (1, 0)
        . putPiece (Two, Spider0) (0, 1)
        . putPiece (One, Ant0) (0, 0)
        $ emptyBoard

style :: String
style = $(do
  cwd <- runIO getCurrentDirectory
  let sassFile = cwd </> "static/style.scss"
  result <- runIO $ readProcess "sass" [sassFile] ""
  addDependentFile sassFile
  lift result
  )

{- Game Widgets -}

-- | When the game starts, this box will appear, prompting each player what
-- their starting piece will be.
startBox :: MonadWidget t m => Player -> m [Event t PieceType]
startBox player =
  zipWithM renderPiece' [0..]
    [ (Bee, BeeType)
    , (Ant0, AntType)
    , (Grass0, GrasshopperType)
    , (Beetle0, BeetleType)
    , (Spider0, SpiderType)
    ]
  where
    renderPiece' :: MonadWidget t m => Int -> (Piece, PieceType) -> m (Event t PieceType)
    renderPiece' i (piece, pieceType) = do
      let coords = toSVGCoord (i, i `div` 2)
      (e, _) <- svg' "g" $ renderPiece coords (player, piece)
      return $ pieceType <$ domEvent Click e
