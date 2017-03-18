module TestHelpers
    ( RiggedDie(..)
    , allDirections
    , boardCounterexample
    , combine
    , arbitraryDie
    , forAllVisibleBoards
    , hasSpaceToMoveLeft
    , hasSpaceToMoveUp
    , isPlayer
    , playerX
    , playerY
    , rollsOf
    , attackRolls
    , damageRolls
    , visibleBoard
    ) where

import           Board
import           Dice
import           Model
import           RequestHandling

import           System.Random
import           Test.QuickCheck

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

hasSpaceToMoveLeft :: Board -> Bool
hasSpaceToMoveLeft board@(Board beforeTiles) =
    visibleBoard width height &&
    beforeTiles !! playerY board !! (playerX board - 1) == Grass Nothing
  where
    width = length beforeTiles
    height = length (head beforeTiles ++ [])

hasSpaceToMoveUp :: Board -> Bool
hasSpaceToMoveUp board@(Board beforeTiles) =
    visibleBoard width height &&
    beforeTiles !! (playerY board - 1) !! playerX board == Grass Nothing
  where
    width = length beforeTiles
    height = length (head beforeTiles ++ [])

isPlayer :: Tile -> Bool
isPlayer tile =
  case tile of
    Wall              -> False
    Grass Nothing     -> False
    Grass (Just char) -> piece char == Piece '@'

playerX :: Board -> Int
playerX = fst . playerCoords

playerY :: Board -> Int
playerY = snd . playerCoords

playerCoords :: Board -> Point
playerCoords b = (x, y)
  where
    (((x, y), _):_) = (filter isCharacter . concat . tilesWithCoords) b

isCharacter :: (Point, Tile) -> Bool
isCharacter (_, Grass (Just _)) = True
isCharacter _                   = False

tilesWithCoords :: Board -> [[(Point, Tile)]]
tilesWithCoords (Board b) = zipWith rowWithCoords [0 ..] b
  where
    rowWithCoords y = zipWith (tileWithCoords y) [0 ..]
    tileWithCoords y x tile = ((x, y), tile)

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
