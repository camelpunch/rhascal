module Keyboard
    ( requestFromKey
    ) where

import Model (Request(..))

requestFromKey :: Char -> Request
requestFromKey 'h' = MoveLeft
requestFromKey 'j' = MoveDown
requestFromKey 'k' = MoveUp
requestFromKey 'l' = MoveRight
requestFromKey _ = DoNothing
