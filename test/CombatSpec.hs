module CombatSpec ( main, spec ) where

import           Test.Hspec
import           Test.QuickCheck

import           ArbitraryTypes  ()
import           TestHelpers

import           Combat

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "battle between two characters" $ do
    specify "defender is damaged when attacker hits" $ property $ \attacker defender ->
      forAll attackRolls $ \attackRoll -> forAll damageRolls $ \damageRoll ->
        let attackDie = RiggedDie [attackRoll, 1]
            damageDie = RiggedDie [damageRoll]
            [_, defenderAfter] = battle attackDie damageDie [attacker, defender]
            hp = hitPoints defender
            hp' = hitPoints defenderAfter
        in  attackRoll >= armourClass defender ==>
              hp' === hp - damageRoll

    specify "defender counters when attacker misses" $ property $ \attacker defender ->
      let hp = hitPoints attacker
          hp' = hitPoints attackerAfter
          attackerArmour = armourClass attacker
          counterDamage = 4
          counterRoll = attackerArmour
          attackDie = RiggedDie [1, counterRoll]
          damageDie = RiggedDie [counterDamage]
          [attackerAfter, _] = battle attackDie damageDie [attacker, defender]
      in  attackerArmour > 1 && attackerArmour < 20 ==>
            hp' === hp - counterDamage

    specify "defender gets two counters on critical hit" $ property $ \attacker defender ->
      let counterDamage = [4, 5]
          attackDie = RiggedDie [1, 20]
          damageDie = RiggedDie counterDamage
          [attackerAfter, _] = battle attackDie damageDie [attacker, defender]
          hp = hitPoints attacker
          hpAfter = hitPoints attackerAfter
      in  hpAfter === hp - sum counterDamage
