{-# LANGUAGE OverloadedStrings #-}

module Hive.Client (app) where

import React.Flux

app :: ReactView ()
app = defineView "Main Hive app" $ \() -> return ()
