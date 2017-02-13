module TestHelpers
    ( RiggedDie(..)
    , allDirections
    , boardCounterexample
    , combine
    , arbitraryDie
    , rollsOf
    , attackRolls
    , damageRolls
    , visibleBoard
    ) where

import Dice
import Model
import RequestHandling

import Test.QuickCheck

-- QuickCheck
combine
    :: Show a
    => Gen a -> Gen a -> (a -> a -> Property) -> Property
combine x y f = forAll x $ \n -> forAll y $ \m -> f n m

-- Board
boardCounterexample :: Board -> Board -> Property -> Property
boardCounterexample before after =
    counterexample $
    "\nBEFORE\n\n" ++ show before ++ "\nAFTER\n\n" ++ show after

allDirections :: Board -> Board
allDirections =
    handleRequest MoveDown .
    handleRequest MoveUp . handleRequest MoveRight . handleRequest MoveLeft

-- Dice
newtype RiggedDie =
    RiggedDie [Roll]

instance Die RiggedDie where
    rollsOf (RiggedDie ns) = ns

arbitraryDie :: [Positive Roll] -> RiggedDie
arbitraryDie rolls = RiggedDie $ map getPositive rolls

attackRolls :: Gen Int
attackRolls = choose (2, 19)

damageRolls :: Gen Int
damageRolls = choose (1, 4)

visibleBoard :: Int -> Int -> Bool
visibleBoard w h = w >= 3 && h >= 3
