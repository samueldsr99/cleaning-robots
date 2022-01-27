-- Utils functions
module Utils where

import Data.List (find)
import System.Random (Random (random, randomR), StdGen, mkStdGen)
import Types (Child (Child), Corral (Corral), Dirt (Dirt), Environment (..), Robot (..))

_getRandomCellsInSquare :: Int -> Int -> [(Int, Int)] -> Int -> StdGen -> ([(Int, Int)], StdGen)
_getRandomCellsInSquare n m currentList k gen =
  let possibles = [(x, y) | x <- [0 .. n - 1], y <- [0 .. m - 1], (x, y) `notElem` currentList]
      (indx, new_gen) = randomR (0, length possibles -1) gen :: (Int, StdGen)
      (i, j) = possibles !! indx
      (rest, new_gen1) = _getRandomCellsInSquare n m ((i, j) : currentList) (k - 1) new_gen
   in if k > 0 && possibles /= []
        then (rest, new_gen1)
        else (currentList, gen)

getRandomCellsInSquare :: Int -> Int -> Int -> Int -> [(Int, Int)]
getRandomCellsInSquare n m amount seed =
  let (_, gen) = random (mkStdGen seed) :: (Int, StdGen)
      (positions, _) = _getRandomCellsInSquare n m [] amount gen
   in positions

_getRandomCellsInSquareNotContaining :: Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> Int -> StdGen -> ([(Int, Int)], StdGen)
_getRandomCellsInSquareNotContaining n m currentList notContainList k gen =
  let possibles =
        [ (x, y)
          | x <- [0 .. n - 1],
            y <- [0 .. m - 1],
            (x, y) `notElem` currentList,
            (x, y) `notElem` notContainList
        ]
      (indx, new_gen) = randomR (0, length possibles - 1) gen :: (Int, StdGen)
      (i, j) = possibles !! indx
      (rest, new_gen1) =
        _getRandomCellsInSquareNotContaining n m ((i, j) : currentList) notContainList (k - 1) new_gen
   in if k > 0 && possibles /= []
        then (rest, new_gen1)
        else (currentList, gen)

getRandomCellsInSquareNotContaining :: Int -> Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
getRandomCellsInSquareNotContaining n m amount seed notContainList =
  let (_, gen) = random (mkStdGen seed) :: (Int, StdGen)
      (positions, _) = _getRandomCellsInSquareNotContaining n m [] notContainList amount gen
   in positions

getRobotsPositions :: [Robot] -> [(Int, Int)]
getRobotsPositions = map position

getChildrenPositions :: [Child] -> [(Int, Int)]
getChildrenPositions = map (\(Child a b) -> (a, b))

getDirtPositions :: [Dirt] -> [(Int, Int)]
getDirtPositions = map (\(Dirt a b) -> (a, b))

getCorralsPositions :: [Corral] -> [(Int, Int)]
getCorralsPositions = map (\(Corral a b) -> (a, b))

-- return robot in cell (r, c) or NULL
robotInCell :: Environment -> Int -> Int -> Maybe Robot
robotInCell env r c =
  let robotsList = robots env
   in find (\Robot {position = (x, y)} -> r == x && c == y) robotsList
