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

import           Dice
import           Model

type Attacker = Character
type Defender = Character

data CombatAction
  = Hit
  | CriticalHit
  | Miss
  | Invalid
  deriving (Eq, Show)

battle :: Die a => a -> a -> [Character] -> [Character]
battle _ _ [] = []
battle _ _ [x] = [x]
battle attackDie damageDie (attacker:defender:_) =
  battleWithRolls (rollsOf attackDie) (rollsOf damageDie) [attacker, defender]

battleWithRolls :: [Roll] -> [Roll] -> [Character] -> [Character]
battleWithRolls (attackRoll:counterRoll:_) damageRolls (attacker:defender:_) =
  case action of
    Miss -> [damage damageRolls counterAction attacker, defender]
    Hit  -> [attacker, damage damageRolls action defender]
    _    -> [attacker, defender]
  where
    action = combatAction attackRoll defender
    counterAction = combatAction counterRoll attacker
battleWithRolls _ _ battlers = battlers

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
