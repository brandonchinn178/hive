-- | Module adapted from https://github.com/reflex-frp/reflex-dom-contrib/blob/master/src/Reflex/Dom/Contrib/Widgets/Svg.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Hive.Client.SVG.Contrib
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
svgDynAttr' :: (PostBuild t m, DomBuilder t m) => Text -> Dynamic t (Map Text Text) -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
svgDynAttr' = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

{-# INLINABLE svgDynAttr #-}
svgDynAttr :: (PostBuild t m, DomBuilder t m) => Text -> Dynamic t (Map Text Text) -> m a -> m a
svgDynAttr elementTag attrs child = snd <$> svgDynAttr' elementTag attrs child

{-# INLINABLE svgAttr' #-}
svgAttr' :: (PostBuild t m, DomBuilder t m) => Text -> Map Text Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
svgAttr' elementTag attrs child = svgDynAttr' elementTag (constDyn attrs) child

{-# INLINABLE svgAttr #-}
svgAttr :: (PostBuild t m, DomBuilder t m) => Text -> Map Text Text -> m a -> m a
svgAttr elementTag attrs child = svgDynAttr elementTag (constDyn attrs) child

{-# INLINABLE svg' #-}
svg' :: (PostBuild t m, DomBuilder t m) => Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
svg' elementTag child = svgAttr' elementTag Map.empty child


{-# INLINABLE svg #-}
svg :: (PostBuild t m, DomBuilder t m) => Text -> m a -> m a
svg elementTag child = svgAttr elementTag Map.empty child

svgClass :: (PostBuild t m, DomBuilder t m) => Text -> Text -> m a -> m a
svgClass elementTag c child = svgAttr elementTag ("class" =: c) child
