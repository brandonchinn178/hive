module Hive.Coordinate
  ( Coordinate
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
