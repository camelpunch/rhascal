module Keyboard
    ( requestFromKey
    ) where

import Model (Request(..))

requestFromKey :: Char -> Maybe Request
requestFromKey 'h' = Just MoveLeft
requestFromKey 'j' = Just MoveDown
requestFromKey 'k' = Just MoveUp
requestFromKey 'l' = Just MoveRight
requestFromKey _ = Nothing
