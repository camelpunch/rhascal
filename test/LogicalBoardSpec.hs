module LogicalBoardSpec ( main, spec ) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Function

import           ArbitraryTypes  ()
import           TestHelpers

import           Board
import           Model

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  specify "zero height board is empty" $ property $ \w ->
    createBoard w 0 === Board []

  specify "can retrieve width" $ property $ \(NonNegative w) (Positive h) ->
    let retrieved = width (createBoard w h)
    in  counterexample ("width of " ++ show retrieved ++
                        " is not equal to input: " ++ show w ++
                        " when height is " ++ show h) $
        retrieved == w

  specify "can retrieve height" $ property $ \(Positive w) (NonNegative h) ->
    let retrieved = height (createBoard w h)
    in  counterexample ("height of " ++ show retrieved ++
                        " is not equal to input: " ++ show h ++
                        " when width is " ++ show w) $
        retrieved == h

  specify "first and last rows are wall" $ property $ \w (Positive h) ->
    let Board board = createBoard w h
        wall = replicate w Wall
    in  head board === wall .&&. last board === wall

  specify "first and last columns are wall" $ property $ \(Positive w) h ->
    let firstColumn = map head board
        lastColumn = map last board
        Board board = createBoard w h
        wall = replicate h Wall
    in  firstColumn === wall .&&. lastColumn === wall

  specify "all non-wall tiles start empty" $
    forAllVisibleBoards $ \b ->
      count (/= Wall) b === count isEmptyTile b

  describe "spawning" $ do
    let numPlayers = count (== playerTile)
        playerTile = Grass $ Just player
        jackal = Character { piece = Piece 'j'
                           , hitPoints = 100
                           , armourClass = 100 }

    specify "a character can be spawned" $
      property $ \g -> forAllVisibleBoards $ \b ->
        numPlayers (spawn g player b) === 1

    specify "characters are never spawned on top of each other" $
      property $ \g g' -> forAllVisibleBoards $ \b ->
        numPlayers (b & spawn g player & spawn g' jackal) === 1
