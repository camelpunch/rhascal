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

unusedDie :: RiggedDie
unusedDie = RiggedDie []

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
prop_1IsAMiss x = landing 1 x === Miss

prop_20LandsCriticalHit :: Defender -> Property
prop_20LandsCriticalHit x = landing 20 x === CriticalHit

prop_2To19IsAHitIfGreaterThanOrEqualToTargetArmourClass :: Defender -> Property
prop_2To19IsAHitIfGreaterThanOrEqualToTargetArmourClass x =
    forAll (choose (2, 19)) $ \n ->
        ArmourClass n >= armourClass x ==> landing n x === Hit

prop_2To19IsAMissIfLessThanTargetArmourClass :: Defender -> Property
prop_2To19IsAMissIfLessThanTargetArmourClass x =
    forAll (choose (2, 19)) $ \n ->
        ArmourClass n < armourClass x ==> landing n x === Miss

prop_21PlusIsInvalid :: Defender -> Property
prop_21PlusIsInvalid x =
    forAll (choose (21, 10000)) $ \n -> landing n x === Invalid

-- Combat: damaging a character
prop_HitCausesDamage :: Defender -> Property
prop_HitCausesDamage x =
    hitPoints (damage die Hit x) === HitPoints (initial - 10)
  where
    HitPoints initial = hitPoints x
    die = RiggedDie [10, 1]

prop_CriticalHitCausesTwoRollsOfDamage :: Defender -> Property
prop_CriticalHitCausesTwoRollsOfDamage x =
    hitPoints (damage die CriticalHit x) === HitPoints (initial - 15)
  where
    HitPoints initial = hitPoints x
    die = RiggedDie [5, 10, 999]

prop_NoDamageOnMissOrInvalid :: Defender -> [Positive Roll] -> Property
prop_NoDamageOnMissOrInvalid x rolls =
    forAll (elements [Miss, Invalid]) $ \l ->
        hitPoints (damage (arbitraryDie rolls) l x) === hitPoints x

prop_NoDamageWhenDieNotRolled :: Defender -> Property
prop_NoDamageWhenDieNotRolled x =
    forAll (elements [Hit, CriticalHit]) $ \l ->
        hitPoints (damage unusedDie l x) === hitPoints x

return []

main :: IO ()
main = do
    result <- $quickCheckAll
    if result
        then exitSuccess
        else exitFailure
