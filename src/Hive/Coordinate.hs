module Hive.Coordinate
  ( Coordinate
  , getNeighbors
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

-- | Get the coordinates surrounding the given coordinate.
getNeighbors :: Coordinate -> [Coordinate]
getNeighbors (x, y) =
  [ (x, y + 1)     -- N
  , (x + 1, y + 1) -- NE
  , (x + 1, y)     -- SE
  , (x, y - 1)     -- S
  , (x - 1, y - 1) -- SW
  , (x - 1, y)     -- NW
  ]
