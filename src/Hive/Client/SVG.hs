-- | Module adapted from https://github.com/reflex-frp/reflex-dom-contrib/blob/master/src/Reflex/Dom/Contrib/Widgets/Svg.hs

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Hive.Client.SVG
  ( svgDynAttr'
  , svgDynAttr
  , svgAttr'
  , svgAttr
  , svg'
  , svg
  , svgClass
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Reflex.Dom.Core

{-# INLINABLE svgDynAttr' #-}
svgDynAttr' :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
svgDynAttr' = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

{-# INLINABLE svgDynAttr #-}
svgDynAttr :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m a
svgDynAttr elementTag attrs = fmap snd . svgDynAttr' elementTag attrs

{-# INLINABLE svgAttr' #-}
svgAttr' :: MonadWidget t m => Text -> Map Text Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
svgAttr' elementTag = svgDynAttr' elementTag . constDyn

{-# INLINABLE svgAttr #-}
svgAttr :: MonadWidget t m => Text -> Map Text Text -> m a -> m a
svgAttr elementTag = svgDynAttr elementTag . constDyn

{-# INLINABLE svg' #-}
svg' :: MonadWidget t m => Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
svg' elementTag = svgAttr' elementTag Map.empty

{-# INLINABLE svg #-}
svg :: MonadWidget t m => Text -> m a -> m a
svg elementTag = svgAttr elementTag Map.empty

svgClass :: MonadWidget t m => Text -> Text -> m a -> m a
svgClass elementTag c = svgAttr elementTag ("class" =: c)
