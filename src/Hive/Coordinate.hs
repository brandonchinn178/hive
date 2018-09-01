{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Hive.Coordinate
  ( Coordinate
  , Neighbors(..)
  , getNeighbors
  , toNeighborhood
  , getNeighborhood
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

-- | A coordinate on the Hive board.
--
-- The origin is arbitrary; all that matters are the coordinates'
-- relative positions to each other.
--
-- Assume the pieces will be of the orientation:
--
--    - - -
--  /       \
-- /         \
-- \         /
--  \       /
--    - - -
--
-- The positive x-axis goes to the south east edge and the
-- positive y-axis goes to the north.
type Coordinate = (Int, Int)

-- | Data associated with the neighbors of a coordinate.
data Neighbors a = Neighbors
  { north     :: a
  , northeast :: a
  , southeast :: a
  , south     :: a
  , southwest :: a
  , northwest :: a
  } deriving (Show, Functor)

-- | Get the coordinates surrounding the given coordinate.
getNeighbors :: Coordinate -> Neighbors Coordinate
getNeighbors (x, y) = Neighbors
  { north     = (x    , y + 1)
  , northeast = (x + 1, y + 1)
  , southeast = (x + 1, y    )
  , south     = (x    , y - 1)
  , southwest = (x - 1, y - 1)
  , northwest = (x - 1, y    )
  }

-- | Convert neighbors to a set.
toNeighborhood :: Ord a => Neighbors a -> Set a
toNeighborhood Neighbors{..} = Set.fromList
  [north, northeast, southeast, south, southwest, northwest]

-- | Get the coordinates surrounding the given coordinate, without caring about
-- order.
getNeighborhood :: Coordinate -> Set Coordinate
getNeighborhood = toNeighborhood . getNeighbors
