module TestHelpers
  ( RiggedDie(..)
  , allDirections
  , boardCounterexample
  , boardNextTurn
  , arbitraryDie
  , forAllVisibleBoards
  , hasSpaceToMoveLeft
  , hasSpaceToMoveUp
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

import           Test.QuickCheck

forAllVisibleBoards :: Testable a => (Board -> a) -> Property
forAllVisibleBoards f =
  forAll (choose (3, 50)) $ \w ->
    forAll (choose (3, 50)) $ \h ->
      f $ createBoard w h

boardCounterexample :: Board -> Board -> Property -> Property
boardCounterexample before after =
  counterexample $
  "\nBEFORE\n\n" ++ show before ++ "\nAFTER\n\n" ++ show after

boardNextTurn :: Request -> Board -> Board
boardNextTurn req b = boardAfter $ nextTurn req (initialTurn b)

allDirections :: Turn -> Turn
allDirections =
  nextTurn MoveDown . nextTurn MoveUp . nextTurn MoveRight . nextTurn MoveLeft

hasSpaceToMoveLeft :: Board -> Bool
hasSpaceToMoveLeft board@(Board beforeTiles) =
  visibleBoard (width board) (height board) &&
  beforeTiles !! playerY board !! (playerX board - 1) == Grass Nothing

hasSpaceToMoveUp :: Board -> Bool
hasSpaceToMoveUp board@(Board beforeTiles) =
  visibleBoard (width board) (height board) &&
  beforeTiles !! (playerY board - 1) !! playerX board == Grass Nothing

playerX :: Board -> Int
playerX = fst . playerCoords

playerY :: Board -> Int
playerY = snd . playerCoords

playerCoords :: Board -> Point
playerCoords b = (x, y)
  where
    (((x, y), _):_) = (filter isPlayer . concat . tilesWithCoords) b
    isPlayer (_, Grass (Just ch)) = piece ch == piece player
    isPlayer (_, Grass Nothing)   = False
    isPlayer (_, Wall)            = False

tilesWithCoords :: Board -> [[(Point, Tile)]]
tilesWithCoords (Board b) = zipWith rowWithCoords [0 ..] b
  where
    rowWithCoords y = zipWith (tileWithCoords y) [0 ..]
    tileWithCoords y x tile = ((x, y), tile)

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
