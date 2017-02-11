{-# LANGUAGE TemplateHaskell #-}

import System.Exit hiding (die)
import System.Random

import Test.QuickCheck

import Board
import Combat
import Dice
import RequestHandling

import ArbitraryTypes ()
import TestHelpers

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
    attackerArmour > 1 && attackerArmour < 20 ==> hp' === hp - counterDamage
  where
    HitPoints hp = hitPoints attacker
    HitPoints hp' = hitPoints attackerAfter
    ArmourClass attackerArmour = armourClass attacker
    counterDamage = 4
    counterRoll = attackerArmour
    attackDie = RiggedDie [missRoll, counterRoll]
    damageDie = RiggedDie [counterDamage]
    [attackerAfter, _] = battle attackDie damageDie [attacker, defender]

prop_TwoCountersWhenCriticalHitIsRolled :: Attacker -> Defender -> Property
prop_TwoCountersWhenCriticalHitIsRolled attacker defender =
    hp' === hp - counterDamage1 - counterDamage2
  where
    HitPoints hp = hitPoints attacker
    HitPoints hp' = hitPoints attackerAfter
    counterDamage1 = 4
    counterDamage2 = 5
    attackDie = RiggedDie [missRoll, criticalHitRoll]
    damageDie = RiggedDie [counterDamage1, counterDamage2]
    [attackerAfter, _] = battle attackDie damageDie [attacker, defender]

-- Board display
prop_BoardShowsWithLineBreaks :: Int -> Positive Int -> Positive Int -> Property
prop_BoardShowsWithLineBreaks seed (Positive width) (Positive height) =
    visibleBoard width height ==> length (lines (show board)) === height +
    length ["\n"]
  where
    board = generateBoard g width height
    g = mkStdGen seed

-- Logical board
prop_ZeroHeightBoardIsEmpty :: Int -> Int -> Property
prop_ZeroHeightBoardIsEmpty seed width = board === []
  where
    Board board = generateBoard g width 0
    g = mkStdGen seed

prop_FirstAndLastRowsAreWall :: Int -> Int -> Positive Int -> Property
prop_FirstAndLastRowsAreWall seed width (Positive height) =
    head board === wall .&&. last board == wall
  where
    Board board = generateBoard g width height
    g = mkStdGen seed
    wall = replicate width Wall

prop_FirstAndLastColumnsAreWall :: Int -> Positive Int -> Int -> Property
prop_FirstAndLastColumnsAreWall seed (Positive width) height =
    firstColumn === wall .&&. lastColumn === wall
  where
    firstColumn = map head board
    lastColumn = map last board
    Board board = generateBoard g width height
    g = mkStdGen seed
    wall = replicate height Wall

prop_NonPlayerTilesStartEmpty :: Int -> Positive Int -> Positive Int -> Property
prop_NonPlayerTilesStartEmpty seed (Positive width) (Positive height) =
    visibleBoard width height ==> countEmpties (concat board) === totalTiles - 1
  where
    Board board = generateBoard g width height
    g = mkStdGen seed
    countEmpties = length . filter isEmptyTile
    isEmptyTile tile =
        case tile of
            Wall -> False
            Grass (Just _) -> False
            Grass Nothing -> True
    totalTiles = (width - 2) * (height - 2)

prop_SinglePlayerSpawned :: Int -> Positive Int -> Positive Int -> Property
prop_SinglePlayerSpawned seed (Positive width) (Positive height) =
    visibleBoard width height ==> countPlayers (concat board) === 1
  where
    Board board = generateBoard g width height
    g = mkStdGen seed
    countPlayers = length . filter isPlayer
    isPlayer tile =
        case tile of
            Wall -> False
            Grass (Just char) -> piece char == Piece '@'
            Grass Nothing -> False

-- Manual Movement (usually a player)
prop_MovementInEveryDirectionEndsBackAtStart :: Int -> Int -> Int -> Property
prop_MovementInEveryDirectionEndsBackAtStart seed width height =
    hasSpaceToMoveLeft before && hasSpaceToMoveUp before ==> before === after
  where
    before = generateBoard g width height
    allDirections =
        handleRequest MoveDown .
        handleRequest MoveUp . handleRequest MoveRight . handleRequest MoveLeft
    after = allDirections before
    g = mkStdGen seed

prop_MovingLeftMovesPlayerLeft :: Int
                               -> Positive Int
                               -> Positive Int
                               -> Property
prop_MovingLeftMovesPlayerLeft seed (Positive width) (Positive height) =
    boardCounterexample before after $
    hasSpaceToMoveLeft before ==> playerX after === playerX before - 1
  where
    before = generateBoard g width height
    after = handleRequest MoveLeft before
    g = mkStdGen seed

hasSpaceToMoveLeft :: Board -> Bool
hasSpaceToMoveLeft board =
    visibleBoard width height &&
    beforeTiles !! playerY board !! (playerX board - 1) == Grass Nothing
  where
    Board beforeTiles = board
    width = length beforeTiles
    height = length (head beforeTiles ++ [])

prop_MovingUpMovesPlayerUp :: Int -> Positive Int -> Positive Int -> Property
prop_MovingUpMovesPlayerUp seed (Positive width) (Positive height) =
    boardCounterexample before after $
    hasSpaceToMoveUp before ==> playerY after === playerY before - 1
  where
    before = generateBoard g width height
    after = handleRequest MoveUp before
    g = mkStdGen seed

hasSpaceToMoveUp :: Board -> Bool
hasSpaceToMoveUp board =
    visibleBoard width height &&
    beforeTiles !! ((playerY board) - 1) !! playerX board == Grass Nothing
  where
    Board beforeTiles = board
    width = length beforeTiles
    height = length (head beforeTiles ++ [])

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
isCharacter _ = False

tilesWithCoords :: Board -> [[(Point, Tile)]]
tilesWithCoords (Board b) = zipWith rowWithCoords [0 ..] b
  where
    rowWithCoords y row = zipWith (tileWithCoords y) [0 ..] row
    tileWithCoords y = \x tile -> ((x, y), tile)

-- Automatic Movement (usually a monster)
return []

main :: IO ()
main = do
    result <- $quickCheckAll
    if result
        then exitSuccess
        else exitFailure
