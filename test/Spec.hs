{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import System.Exit hiding (die)
import System.Random

import Combat
import Dice

import ArbitraryTypes ()

newtype RiggedDie =
    RiggedDie [Roll]

instance Die RiggedDie where
    rollsOf (RiggedDie ns) = ns

arbitraryDie :: [Positive Roll] -> RiggedDie
arbitraryDie rolls = RiggedDie $ map getPositive rolls

-- Dice
prop_d4WithinRange :: Int -> Int -> Bool
prop_d4WithinRange seed n = all (`elem` [1 .. 4]) rolls
  where
    g = mkStdGen seed
    rolls = take n $ rollsOf (D4 g)

prop_d20WithinRange :: Int -> Int -> Bool
prop_d20WithinRange seed n = all (`elem` [1 .. 20]) rolls
  where
    g = mkStdGen seed
    rolls = take n $ rollsOf (D20 g)

-- Combat: deciding whether we hit
prop_1IsAMiss :: Defender -> Property
prop_1IsAMiss x = combatAction 1 x === Miss

prop_20LandsCriticalHit :: Defender -> Property
prop_20LandsCriticalHit x = combatAction 20 x === CriticalHit

prop_2To19IsAHitIfGreaterThanOrEqualToTargetArmourClass :: Defender -> Property
prop_2To19IsAHitIfGreaterThanOrEqualToTargetArmourClass x =
    forAll attackRolls $ \n ->
        ArmourClass n >= armourClass x ==> combatAction n x === Hit

prop_2To19IsAMissIfLessThanTargetArmourClass :: Defender -> Property
prop_2To19IsAMissIfLessThanTargetArmourClass x =
    forAll attackRolls $ \n ->
        ArmourClass n < armourClass x ==> combatAction n x === Miss

prop_21PlusIsInvalid :: Defender -> Property
prop_21PlusIsInvalid x =
    forAll (choose (21, 10000)) $ \n -> combatAction n x === Invalid

-- Combat: damaging a character
prop_HitCausesDamage :: Defender -> Property
prop_HitCausesDamage x =
    hitPoints (damage [10, 1] Hit x) === HitPoints (initial - 10)
  where
    HitPoints initial = hitPoints x

prop_CriticalHitCausesTwoRollsOfDamage :: Defender -> Property
prop_CriticalHitCausesTwoRollsOfDamage x =
    hitPoints (damage [5, 10, 999] CriticalHit x) === HitPoints (initial - 15)
  where
    HitPoints initial = hitPoints x

prop_NoDamageOnMissOrInvalid :: Defender -> [Positive Roll] -> Property
prop_NoDamageOnMissOrInvalid x rolls =
    forAll (elements [Miss, Invalid]) $ \l ->
        hitPoints (damage (rollsOf (arbitraryDie rolls)) l x) === hitPoints x

prop_NoDamageWhenDieNotRolled :: Defender -> Property
prop_NoDamageWhenDieNotRolled x =
    forAll (elements [Hit, CriticalHit]) $ \l ->
        hitPoints (damage [] l x) === hitPoints x

-- Combat: battle between two characters
prop_DefenderIsDamagedWhenAttackerHits :: Attacker -> Defender -> Property
prop_DefenderIsDamagedWhenAttackerHits attacker defender =
    combine attackRolls damageRolls $ \attackRoll damageRoll ->
        let attackDie = RiggedDie [attackRoll, 1]
            damageDie = RiggedDie [damageRoll]
            [_, defenderAfter] = battle attackDie damageDie [attacker, defender]
            HitPoints hp = hitPoints defender
            HitPoints hp' = hitPoints defenderAfter
        in ArmourClass attackRoll >=
           armourClass defender ==> hp' === hp - damageRoll

prop_DefenderCountersWhenAttackerMisses :: Attacker -> Defender -> Property
prop_DefenderCountersWhenAttackerMisses attacker defender =
    attackerArmour > 1 && attackerArmour < 20 ==> hp' === hp - 4
  where
    HitPoints hp = hitPoints attacker
    HitPoints hp' = hitPoints attackerAfter
    ArmourClass attackerArmour = armourClass attacker
    attackDie = RiggedDie [1, attackerArmour]
    damageDie = RiggedDie [4]
    [attackerAfter, _] = battle attackDie damageDie [attacker, defender]

combine
    :: Show a
    => Gen a -> Gen a -> (a -> a -> Property) -> Property
combine x y f = forAll x $ \n -> forAll y $ \m -> f n m

attackRolls :: Gen Int
attackRolls = choose (2, 19)

damageRolls :: Gen Int
damageRolls = choose (1, 4)

return []

main :: IO ()
main = do
    result <- $quickCheckAll
    if result
        then exitSuccess
        else exitFailure
