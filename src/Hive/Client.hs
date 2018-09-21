{-# LANGUAGE OverloadedStrings #-}

module Hive.Client (app) where

import Language.Javascript.JSaddle (JSM)
import Reflex.Dom.Core

app :: JSM ()
app = mainWidget $ el "p" $ text "Hello world!"
