module Display
    ( changedLines
    ) where

import Model

changedLines :: Board -> Board -> [Maybe [Tile]]
changedLines (Board before) (Board after) =
    zipWith
        (\x y ->
             if x == y
                 then Nothing
                 else Just y)
        before
        after
