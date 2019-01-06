{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hive.Client (app) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (addDependentFile, lift, runIO)
import Language.Javascript.JSaddle (JSM)
import Reflex.Dom.Core
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.Process (readProcess)

import Hive.Client.SVG
import Hive.Core.Board (emptyBoard, putPiece)
import Hive.Core.Piece (Piece(..))
import Hive.Core.Player (Player(..))

app :: JSM ()
app = mainWidgetWithCss style $ svgAttr "svg" (mconcat svgAttrs) $
  renderBoard
    $ putPiece (One, Bee) (0, -1)
    $ putPiece (Two, Grass0) (1, 2)
    $ putPiece (One, Beetle0) (1, 0)
    $ putPiece (Two, Spider0) (0, 1)
    $ putPiece (One, Ant0) (0, 0)
    $ emptyBoard
  where
    svgAttrs =
      [ "viewBox" =: "0 0 500 500"
      , "width" =: "500"
      , "height" =: "500"
      ]

style :: ByteString
style = $(do
  cwd <- runIO getCurrentDirectory
  let sassFile = cwd </> "static/style.scss"
  result <- runIO $ readProcess "sass" [sassFile] ""
  addDependentFile sassFile
  lift $ pack result
  )
