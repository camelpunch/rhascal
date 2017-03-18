module Damage
  ( CombatAction (..)
  , combatAction
  , damage
  ) where

import           Dice
import           Model

data CombatAction
  = Hit
  | CriticalHit
  | Miss
  | Invalid
  deriving (Eq, Show)

combatAction :: Roll -> Defender -> CombatAction
combatAction roll defender
  | roll == 1 = Miss
  | roll == 20 = CriticalHit
  | roll > 20 = Invalid
  | roll >= armourClass defender = Hit
  | otherwise = Miss

damage :: [Roll] -> CombatAction -> Defender -> Defender
damage (roll:_) Hit x = x {hitPoints = hitPoints x - roll}
damage (roll1:roll2:_) CriticalHit x = x {hitPoints = hitPoints x - roll1 - roll2}
damage [] _ x = x
damage [_] CriticalHit x = x
damage _ Miss x = x
damage _ Invalid x = x
