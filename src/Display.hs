module Display
    ( changedRows
    ) where

import           Model

changedRows :: Board -> Board -> [Maybe [Tile]]
changedRows (Board before) (Board after) =
    zipWith
        (\x y ->
             if x == y
                 then Nothing
                 else Just y)
        before
        after
