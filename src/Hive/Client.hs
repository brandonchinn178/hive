{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hive.Client (app) where

import qualified Data.Text as Text
import Hasmin (minifyCSS)
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
app = mainWidgetWithHead header body

header :: (PostBuild t m, DomBuilder t m) => m ()
header = do
  el "title" $ text "Hive"
  case minifyCSS $ Text.pack style of
    Right css -> el "style" $ text css
    _ -> fail "Bad style.scss file"

body :: (PostBuild t m, DomBuilder t m) => m ()
body = svgAttr "svg" ("viewBox" =: "0 0 1000 1000") $
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
