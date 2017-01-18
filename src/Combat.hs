module Combat
    ( combatAction
    , damage
    , Roll
    , CombatAction(..)
    , Defender
    , module Model
    ) where

import Dice
import Model

type Defender = Character

data CombatAction
    = Hit
    | CriticalHit
    | Miss
    | Invalid
    deriving (Eq, Show)

combatAction :: Roll -> Defender -> CombatAction
combatAction 1 _ = Miss
combatAction 20 _ = CriticalHit
combatAction n x
    | n > 20 = Invalid
    | ArmourClass n >= armourClass x = Hit
    | otherwise = Miss

damage
    :: Die a
    => a -> CombatAction -> Defender -> Defender
damage die l x =
    x
    { hitPoints =
          case l of
              Hit -> HitPoints (initial - roll1)
              CriticalHit -> HitPoints (initial - roll1 - roll2)
              Miss -> HitPoints initial
              Invalid -> HitPoints initial
    }
  where
    (roll1:roll2:_) = rollsOf die ++ repeat 0
    HitPoints initial = hitPoints x
