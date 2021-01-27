module Lightarrow.GridField.CardinalDirection where

import Linear

data CardinalDirection = East | North | South | West
    deriving (Enum, Eq, Ord, Read, Show)

oppositeDirection :: CardinalDirection -> CardinalDirection
oppositeDirection North = South
oppositeDirection West  = East
oppositeDirection South = North
oppositeDirection East  = West

-- | A unit vector pointing in a given cardinal direction.  North is aligned
-- with the \(y\)-axis, and east is aligned with the \(x\)-axis.
displacement :: Num a => CardinalDirection -> V2 a
displacement North  = V2 0 (-1)
displacement West   = V2 (-1) 0
displacement South  = V2 0 1
displacement East   = V2 1 0
