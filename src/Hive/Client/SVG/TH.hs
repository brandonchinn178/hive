{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Hive.Client.SVG.TH (loadSVG) where

import Data.Char (toLower, toUpper)
import Data.Maybe (fromMaybe)
import Graphics.Svg
    ( Document(..)
    , Origin(..)
    , Path(..)
    , PathCommand(..)
    , RPoint
    , Tree(..)
    , loadSvgFile
    )
import Language.Haskell.TH (ExpQ, runIO)
import Language.Haskell.TH.Syntax (Lift(..), addDependentFile)
import Linear.V2 (V2(..))
import System.Directory (getCurrentDirectory)

-- | Load an SVG from a file with Template Haskell.
--
-- The Template Haskell evaluates to [String], representing the paths in the
-- SVG.
loadSVG :: FilePath -> ExpQ
loadSVG fp = do
  -- path to directory with `Setup.hs`
  currDir <- runIO getCurrentDirectory
  let path = currDir ++ "/static/" ++ fp
  addDependentFile path
  svgDoc <- runIO $ loadSvgFile' path
  lift $ parseSVG svgDoc
  where
    loadSvgFile' path =
      fromMaybe (error $ "Could not parse: " ++ path) <$> loadSvgFile path

-- | Parse an SVG loaded in by svg-tree into an SVG_El from reflex-dom-svg.
parseSVG :: Document -> [String]
parseSVG = map fromElement . _elements
  where
    fromElement = \case
      PathTree (Path _ commands) -> unwords . map parsePathCommand $ commands
      _ -> error "Found a non <path> element"

parsePathCommand :: PathCommand -> String
parsePathCommand = \case
  MoveTo o points ->                       cmd "M" fromRPoint o points
  LineTo o points ->                       cmd "L" fromRPoint o points
  HorizontalTo o coords ->                 cmd "H" fromVal o coords
  VerticalTo o coords ->                   cmd "V" fromVal o coords
  CurveTo o points ->                      cmd "C" fromRPoint3 o points
  SmoothCurveTo o points ->                cmd "S" fromRPoint2 o points
  QuadraticBezier o points ->              cmd "Q" fromRPoint2 o points
  SmoothQuadraticBezierCurveTo o points -> cmd "T" fromRPoint o points
  EllipticalArc o info ->                  cmd "A" fromArcInfo o info
  EndPath ->                                   "Z"
  where
    cmd c f o xs =
      let c' = case o of
            OriginRelative -> map toLower c
            OriginAbsolute -> map toUpper c
      in unwords . (c' :) . concatMap f $ xs
    fromArcInfo (rx,ry,angle,isLargeArc,isSweep,dest) = concat
      [ fromVal rx
      , fromVal ry
      , fromVal angle
      , fromBool isLargeArc
      , fromBool isSweep
      , fromRPoint dest
      ]

fromRPoint :: RPoint -> [String]
fromRPoint (V2 x y) = [show x ++ "," ++ show y]

fromRPoint2 :: (RPoint, RPoint) -> [String]
fromRPoint2 (p1, p2) = concatMap fromRPoint [p1, p2]

fromRPoint3 :: (RPoint, RPoint, RPoint) -> [String]
fromRPoint3 (p1, p2, p3) = concatMap fromRPoint [p1, p2, p3]

fromVal :: (Show a, Num a) => a -> [String]
fromVal = (:[]) . show

fromBool :: Bool -> [String]
fromBool b = [if b then "1" else "0"]
