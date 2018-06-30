{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Hive.Coordinate
  ( Coordinate
  , Neighbors(..)
  , getNeighbors
  , toNeighborhood
  , canSlide
  ) where

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
  { north = (x, y + 1)
  , northeast = (x + 1, y + 1)
  , southeast = (x + 1, y)
  , south = (x, y - 1)
  , southwest = (x - 1, y - 1)
  , northwest = (x - 1, y)
  }

-- | Convert neighbors to a list.
toNeighborhood :: Neighbors a -> [a]
toNeighborhood Neighbors{..} = [north, northeast, southeast, south, southwest, northwest]

-- | Return whether the given piece can physically slide out of the given configuration.
--
-- The elements in the given list are True if that spot (corresponding with 'getNeighbors') is
-- occupied.
canSlide :: Neighbors Bool -> Bool
canSlide Neighbors{..} = or $ fmap and trappedIfMatches
  where
    trappedIfMatches =
      [ [north, southeast, southwest]
      , [northeast, south, northwest]
      , [north, northeast, south, southwest]
      , [northeast, southeast, southwest, northwest]
      , [southeast, south, northwest, north]
      ]
