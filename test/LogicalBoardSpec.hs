module LogicalBoardSpec ( main, spec ) where

import           Test.Hspec
import           Test.QuickCheck

import           ArbitraryTypes  ()
import           TestHelpers

import           Board
import           Model

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  specify "zero height board is empty" $ property $ \g w ->
    generateBoard g w 0 === Board []

  specify "can retrieve width" $ property $ \g (NonNegative w) (Positive h) ->
    let retrieved = width (generateBoard g w h)
    in  counterexample ("width of " ++ show retrieved ++
                        " is not equal to input: " ++ show w ++
                        " when height is " ++ show h) $
        retrieved == w

  specify "can retrieve height" $ property $ \g (Positive w) (NonNegative h) ->
    let retrieved = height (generateBoard g w h)
    in  counterexample ("height of " ++ show retrieved ++
                        " is not equal to input: " ++ show h ++
                        " when width is " ++ show w) $
        retrieved == h

  specify "first and last rows are wall" $ property $ \g w (Positive h) ->
    let Board board = generateBoard g w h
        wall = replicate w Wall
    in  head board === wall .&&. last board === wall

  specify "first and last columns are wall" $ property $ \g (Positive w) h ->
    let firstColumn = map head board
        lastColumn = map last board
        Board board = generateBoard g w h
        wall = replicate h Wall
    in  firstColumn === wall .&&. lastColumn === wall

  specify "non-player tiles start empty" $
    forAllVisibleBoards $ \b ->
      let isEmptyTile tile =
            case tile of
              Wall           -> False
              Grass (Just _) -> False
              Grass Nothing  -> True
          totalTiles = (width b - 2) * (height b - 2)
      in  count isEmptyTile b === totalTiles - 1

  specify "only one player is spawned" $
    forAllVisibleBoards $ \b ->
      count isPlayer b === 1
