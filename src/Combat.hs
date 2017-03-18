module Combat
  ( battle
  , Roll
  , module Model
  ) where

import           Damage
import           Dice
import           Model

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
