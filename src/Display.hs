module Display
    ( changedLines
    ) where

import Model

changedLines :: Board -> Board -> [(Bool, [Tile])]
changedLines (Board before) (Board after) =
    zipWith (\x y -> (x /= y, y)) before after
