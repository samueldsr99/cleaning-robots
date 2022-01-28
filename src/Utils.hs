-- Utils functions
module Utils where

import Data.Dynamic (Dynamic)
import Data.List (find, findIndex)
import Data.Maybe (fromJust, isJust, isNothing)
import System.Random
  ( Random (random, randomR),
    StdGen,
    mkStdGen,
  )
import Types
  ( Child (Child),
    ChildAction (..),
    Corral (Corral),
    Direction (..),
    Dirt (Dirt),
    Environment (..),
    Obstacle (..),
    Robot (..),
  )

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

getRobotInCell :: Environment -> Int -> Int -> Maybe Robot
getRobotInCell env r c =
  let robotsList = robots env
   in find (\Robot {position = (x, y)} -> (r, c) == (x, y)) robotsList

getChildInCell :: Environment -> Int -> Int -> Maybe Child
getChildInCell env r c =
  let childrenList = children env
   in find (\(Child x y) -> (r, c) == (x, y)) childrenList

getDirtInCell :: Environment -> Int -> Int -> Maybe Dirt
getDirtInCell env r c =
  let dirtList = dirt env
   in find (\(Dirt x y) -> r == x && c == y) dirtList

getCorralInCell :: Environment -> Int -> Int -> Maybe Corral
getCorralInCell env r c =
  let corralsList = corrals env
   in find (\(Corral x y) -> r == x && c == y) corralsList

getObstacleInCell :: Environment -> Int -> Int -> Maybe Obstacle
getObstacleInCell env r c =
  let obstaclesList = obstacles env
   in find (\(Obstacle x y) -> r == x && c == y) obstaclesList

isCellInRange :: Int -> Int -> Environment -> Bool
isCellInRange r c Environment {n = n, m = m} =
  r >= 0 && r < n && c >= 0 && c < m

isCellFree :: (Int, Int) -> Environment -> Bool
isCellFree (r, c) env =
  let robotInCell = getRobotInCell env r c
      childInCell = getChildInCell env r c
      dirtInCell = getDirtInCell env r c
      corralInCell = getCorralInCell env r c
      obstacleInCell = getObstacleInCell env r c
   in isCellInRange r c env
        && isNothing robotInCell
        && isNothing childInCell
        && isNothing dirtInCell
        && isNothing corralInCell
        && isNothing obstacleInCell

-- Check if child can move obstacle in a direction given the obstacle index
canMoveObstacle :: (Environment, Int) -> Direction -> Bool
canMoveObstacle (env, index) direction =
  let obstacle = obstacles env !! index
      obstaclePosition = (\(Obstacle r c) -> (r, c)) obstacle
      newCell = adjacentCell env obstaclePosition direction
      obstacleIndexInCell =
        if isNothing newCell
          then Nothing
          else findIndex (\(Obstacle oR oC) -> (oR, oC) == fromJust newCell) (obstacles env)
   in isJust newCell
        && ( isCellFree (fromJust newCell) env
               || ( isJust obstacleIndexInCell && canMoveObstacle (env, fromJust obstacleIndexInCell) direction
                  )
           )

-- A child can move if cell is not occupied by Robot | Child | Dirt | Corral
canMoveChild :: (Environment, Int) -> Direction -> Bool
canMoveChild (env, index) direction =
  let child = children env !! index
      childPosition = (\(Child x y) -> (x, y)) child
      newCell = adjacentCell env childPosition direction
   in isJust newCell
        && ( let (r, c) = fromJust newCell
                 robotInCell = getRobotInCell env r c
                 childInCell = getChildInCell env r c
                 dirtInCell = getDirtInCell env r c
                 corralInCell = getCorralInCell env r c
                 obstacleInCell = getObstacleInCell env r c
                 obstacleIndex =
                   if isNothing obstacleInCell
                     then Nothing
                     else findIndex (\o -> o == fromJust obstacleInCell) (obstacles env)
              in isCellInRange r c env
                   && isNothing robotInCell
                   && isNothing childInCell
                   && isNothing dirtInCell
                   && isNothing corralInCell
                   && (isNothing obstacleInCell || canMoveObstacle (env, fromJust obstacleIndex) direction)
           )

adjacentCell :: Environment -> (Int, Int) -> Direction -> Maybe (Int, Int)
adjacentCell env (r, c) dir
  | dir == DUp = Just (r - 1, c)
  | dir == DRight = Just (r, c + 1)
  | dir == DDown = Just (r + 1, c)
  | dir == DLeft = Just (r, c - 1)
  | otherwise = Nothing

childActionToDirection :: ChildAction -> Maybe Direction
childActionToDirection action
  | action == CUp = Just DUp
  | action == CRight = Just DRight
  | action == CDown = Just DDown
  | action == CLeft = Just DLeft
  | otherwise = Nothing

-- replace an element in a iterable with another
replace :: Int -> [a] -> a -> [a]
replace _ [] _ = []
replace 0 (_ : xs) a = a : xs
replace elemIndex (x : xs) newElem =
  if elemIndex < 0
    then x : xs
    else x : replace (elemIndex - 1) xs newElem
