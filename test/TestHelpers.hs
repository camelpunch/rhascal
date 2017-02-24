module TestHelpers
    ( RiggedDie(..)
    , allDirections
    , boardCounterexample
    , combine
    , arbitraryDie
    , forAllVisibleBoards
    , rollsOf
    , attackRolls
    , damageRolls
    , visibleBoard
    ) where

import Board
import Dice
import Model
import RequestHandling

import System.Random
import Test.QuickCheck

-- QuickCheck
combine
    :: Show a
    => Gen a -> Gen a -> (a -> a -> Property) -> Property
combine x y f = forAll x $ \n -> forAll y $ \m -> f n m

forAllVisibleBoards :: (Board -> Property) -> Property
forAllVisibleBoards f =
    forAll (choose (1, 1000)) $ \seed ->
        forAll (choose (3, 50)) $ \width ->
            forAll (choose (3, 50)) $ \height ->
                let g = mkStdGen seed
                in f $ generateBoard g width height

-- Board
boardCounterexample :: Board -> Board -> Property -> Property
boardCounterexample before after =
    counterexample $
    "\nBEFORE\n\n" ++ show before ++ "\nAFTER\n\n" ++ show after

allDirections :: Board -> Board
allDirections =
    nextTurn MoveDown . nextTurn MoveUp . nextTurn MoveRight . nextTurn MoveLeft

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
