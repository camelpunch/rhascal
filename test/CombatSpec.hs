module CombatSpec ( main, spec ) where

import           Test.Hspec
import           Test.QuickCheck

import           ArbitraryTypes  ()
import           TestHelpers

import           Combat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "deciding whether we hit" $ do
    specify "1 is a miss" $ property $ \x ->
      combatAction 1 x === Miss

    specify "20 lands a critical hit" $ property $ \x ->
      combatAction 20 x === CriticalHit

    specify "2-19 is a hit if at least equals target armour class" $ property $ \x ->
      forAll attackRolls $ \n ->
        ArmourClass n >= armourClass x ==> combatAction n x === Hit

    specify "2-19 is a miss if less than target armour class" $ property $ \x ->
      forAll attackRolls $ \n ->
        ArmourClass n < armourClass x ==> combatAction n x === Miss

    specify "21+ is invalid" $ property $ \x ->
      forAll (choose (21, 10000)) $ \n -> combatAction n x === Invalid

  describe "damaging a character" $ do
    specify "hit causes damage" $ property $ \x ->
      let HitPoints initial = hitPoints x
      in  hitPoints (damage [10, 1] Hit x) === HitPoints (initial - 10)

    specify "critical hit causes two rolls of damage" $ property $ \x ->
      let HitPoints initial = hitPoints x
      in  hitPoints (damage [5, 10, 999] CriticalHit x) === HitPoints (initial - 15)

    specify "no damage on miss or invalid" $ property $ \x rolls ->
      forAll (elements [Miss, Invalid]) $ \l ->
        hitPoints (damage (rollsOf (arbitraryDie rolls)) l x) === hitPoints x

    specify "no damage when die not rolled" $ property $ \x ->
      forAll (elements [Hit, CriticalHit]) $ \l ->
        hitPoints (damage [] l x) === hitPoints x

  describe "battle between two characters" $ do
    specify "defender is damaged when attacker hits" $ property $ \attacker defender ->
      combine attackRolls damageRolls $ \attackRoll damageRoll ->
        let attackDie = RiggedDie [attackRoll, 1]
            damageDie = RiggedDie [damageRoll]
            [_, defenderAfter] = battle attackDie damageDie [attacker, defender]
            HitPoints hp = hitPoints defender
            HitPoints hp' = hitPoints defenderAfter
        in  ArmourClass attackRoll >= armourClass defender ==>
              hp' === hp - damageRoll

    specify "defender counters when attacker misses" $ property $ \attacker defender ->
      let HitPoints hp = hitPoints attacker
          HitPoints hp' = hitPoints attackerAfter
          ArmourClass attackerArmour = armourClass attacker
          counterDamage = 4
          counterRoll = attackerArmour
          attackDie = RiggedDie [missRoll, counterRoll]
          damageDie = RiggedDie [counterDamage]
          [attackerAfter, _] = battle attackDie damageDie [attacker, defender]
      in  attackerArmour > 1 && attackerArmour < 20 ==>
            hp' === hp - counterDamage

    specify "defender gets two counters on critical hit" $ property $ \attacker defender ->
      let counterDamage = [4, 5]
          attackDie = RiggedDie [missRoll, criticalHitRoll]
          damageDie = RiggedDie counterDamage
          [attackerAfter, _] = battle attackDie damageDie [attacker, defender]
          HitPoints hp = hitPoints attacker
          HitPoints hpAfter = hitPoints attackerAfter
      in  hpAfter === hp - sum counterDamage
