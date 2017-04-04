module Display
    ( changedRows
    ) where

import           Model

changedRows :: Board -> Board -> [Maybe [Tile]]
changedRows (Board before) (Board after) =
  zipWith (\row1 row2 -> if row1 == row2 then Nothing else Just row2)
          before after
