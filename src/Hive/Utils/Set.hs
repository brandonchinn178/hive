module Hive.Utils.Set
  ( any
  , catMaybes
  ) where

import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (any)

any :: Ord a => (a -> Bool) -> Set a -> Bool
any f = not . Set.null . Set.filter f

catMaybes :: Ord a => Set (Maybe a) -> Set a
catMaybes = Set.map fromJust . Set.filter isJust
