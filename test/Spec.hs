import Test.Hspec
import Test.QuickCheck

import System.Random

import Combat
import Model

instance Arbitrary Character where
    arbitrary = do
        Positive hp <- arbitrary
        Positive ac <- arbitrary
        c <- arbitrary
        Positive x <- arbitrary
        Positive y <- arbitrary
        return $
            Character
            { hitPoints = HitPoints hp
            , armourClass = ArmourClass ac
            , piece = Piece c
            , coords = Point (x, y)
            }

main :: IO ()
main =
    hspec $ do
        describe "attack rolls" $ do
            specify "1 is a miss" $ property $ \x -> landing 1 x == Miss
            specify "20 lands a critical hit" $ property $ \x ->
                landing 20 x == CriticalHit
            specify "2-19 when >= target's armour class lands a hit" $
                forAll (choose (2, 19)) $ \n x ->
                ArmourClass n >= armourClass x ==> landing n x == Hit
            specify "2-19 when < target's armour class is a miss" $
                forAll (choose (2, 19)) $ \n x ->
                ArmourClass n < armourClass x ==> landing n x == Miss
