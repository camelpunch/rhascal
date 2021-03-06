module DamageSpec ( main, spec ) where

import           Test.Hspec
import           Test.QuickCheck

import           ArbitraryTypes  ()
import           TestHelpers

import           Damage
import           Model

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
        n >= armourClass x ==> combatAction n x === Hit

    specify "2-19 is a miss if less than target armour class" $ property $ \x ->
      forAll attackRolls $ \n ->
        n < armourClass x ==> combatAction n x === Miss

    specify "21+ is invalid" $ property $ \x ->
      forAll (choose (21, 10000)) $ \n -> combatAction n x === Invalid

  describe "damaging a character" $ do
    specify "hit causes damage" $ property $ \x ->
      let initial = hitPoints x
      in  hitPoints (damage [10, 1] Hit x) === initial - 10

    specify "critical hit causes two rolls of damage" $ property $ \x ->
      let initial = hitPoints x
      in  hitPoints (damage [5, 10, 999] CriticalHit x) === initial - 15

    specify "no damage on miss or invalid" $ property $ \x rolls ->
      forAll (elements [Miss, Invalid]) $ \l ->
        hitPoints (damage (rollsOf (arbitraryDie rolls)) l x) === hitPoints x

    specify "no damage when die not rolled" $ property $ \x ->
      forAll (elements [Hit, CriticalHit]) $ \l ->
        hitPoints (damage [] l x) === hitPoints x
