module Combat
    ( battle
    , combatAction
    , damage
    , Roll
    , CombatAction(..)
    , Attacker
    , Defender
    , module Model
    ) where

import Dice
import Model

type Attacker = Character

type Defender = Character

data CombatAction
    = Hit
    | CriticalHit
    | Miss
    | Invalid
    deriving (Eq, Show)

battle
    :: Die a
    => a -> [Character] -> [Character]
battle _ [] = []
battle _ [x] = [x]
battle die (attacker:defender:_) =
    case combatAction attackRoll defender of
        Hit -> [attacker, defender {hitPoints = HitPoints 0}]
        CriticalHit -> [attacker, defender]
        Miss -> [attacker {hitPoints = HitPoints 0}, defender]
        Invalid -> [attacker, defender]
  where
    (attackRoll:_) = rollsOf die

combatAction :: Roll -> Defender -> CombatAction
combatAction 1 _ = Miss
combatAction 20 _ = CriticalHit
combatAction roll defender
    | roll > 20 = Invalid
    | ArmourClass roll >= armourClass defender = Hit
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
