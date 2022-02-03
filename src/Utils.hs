-- Utils functions
module Utils where

import Data.Dynamic (Dynamic)
import Data.List (find, findIndex)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import System.Random
  ( Random (random, randomR, randomRs),
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
    RobotAction (..),
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

getRandomCellsInSquare :: Int -> Int -> Int -> StdGen -> ([(Int, Int)], StdGen)
getRandomCellsInSquare n m amount gen =
  let (_, newGen) = random gen :: (Int, StdGen)
      (positions, _) = _getRandomCellsInSquare n m [] amount gen
   in (positions, newGen)

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

getRandomCellsInSquareNotContaining :: Int -> Int -> Int -> StdGen -> [(Int, Int)] -> ([(Int, Int)], StdGen)
getRandomCellsInSquareNotContaining n m amount gen notContainList =
  let (_, newGen) = random gen :: (Int, StdGen)
      (positions, _) = _getRandomCellsInSquareNotContaining n m [] notContainList amount gen
   in (positions, newGen)

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

-- Check whether a child can move
canMoveChild :: (Environment, Int) -> Direction -> Bool
canMoveChild (env, index) direction =
  let child = children env !! index
      (r, c) = (\(Child x y) -> (x, y)) child
      newCell = adjacentCell env (r, c) direction
      corralInCell = getCorralInCell env r c
   in isNothing corralInCell
        && ( isJust newCell
               && ( let (newR, newC) = fromJust newCell
                        robotInCell = getRobotInCell env newR newC
                        childInCell = getChildInCell env newR newC
                        dirtInCell = getDirtInCell env newR newC
                        corralInCell = getCorralInCell env newR newC
                        obstacleInCell = getObstacleInCell env newR newC
                        obstacleIndex =
                          if isNothing obstacleInCell
                            then Nothing
                            else findIndex (\o -> o == fromJust obstacleInCell) (obstacles env)
                     in isCellInRange newR newC env
                          && not (childIsLoaded env index)
                          && isNothing robotInCell
                          && isNothing childInCell
                          && isNothing dirtInCell
                          && isNothing corralInCell
                          && (isNothing obstacleInCell || canMoveObstacle (env, fromJust obstacleIndex) direction)
                  )
           )

-- Check whether a robot can move
canMoveRobot :: (Environment, Int) -> Direction -> Bool
canMoveRobot (env, index) direction =
  let robot = robots env !! index
      robotPosition = position robot
      newCell = adjacentCell env robotPosition direction
   in isJust newCell
        && ( let (r, c) = fromJust newCell
                 robotInCell = getRobotInCell env r c
                 obstacleInCell = getObstacleInCell env r c
              in isCellInRange r c env
                   && isNothing robotInCell
                   && isNothing obstacleInCell
           )

canMoveRobotInPosition :: Environment -> (Int, Int) -> Direction -> Bool
canMoveRobotInPosition env (r, c) direction =
  let newCell = adjacentCell env (r, c) direction
   in isJust newCell
        && ( let (r, c) = fromJust newCell
                 robotInCell = getRobotInCell env r c
                 obstacleInCell = getObstacleInCell env r c
              in isCellInRange r c env
                   && isNothing robotInCell
                   && isNothing obstacleInCell
           )

adjacentCell :: Environment -> (Int, Int) -> Direction -> Maybe (Int, Int)
adjacentCell env (r, c) dir
  | dir == DUp = Just (r - 1, c)
  | dir == DRight = Just (r, c + 1)
  | dir == DDown = Just (r + 1, c)
  | dir == DLeft = Just (r, c - 1)
  | dir == DDiagonalUpLeft = Just (r - 1, c - 1)
  | dir == DDiagonalUpRight = Just (r - 1, c + 1)
  | dir == DDiagonalDownLeft = Just (r + 1, c - 1)
  | dir == DDiagonalDownRight = Just (r + 1, c + 1)
  | otherwise = Nothing

childActionToDirection :: ChildAction -> Maybe Direction
childActionToDirection action
  | action == CUp = Just DUp
  | action == CRight = Just DRight
  | action == CDown = Just DDown
  | action == CLeft = Just DLeft
  | otherwise = Nothing

robotActionToDirection :: RobotAction -> Maybe Direction
robotActionToDirection action
  | action == RUp = Just DUp
  | action == RRight = Just DRight
  | action == RDown = Just DDown
  | action == RLeft = Just DLeft
  | otherwise = Nothing

robotDirectionToAction :: Direction -> Maybe RobotAction
robotDirectionToAction direction
  | direction == DUp = Just RUp
  | direction == DRight = Just RRight
  | direction == DDown = Just RDown
  | direction == DLeft = Just RLeft
  | otherwise = Nothing

-- replace an element in a iterable with another
replace :: Int -> [a] -> a -> [a]
replace _ [] _ = []
replace 0 (_ : xs) a = a : xs
replace elemIndex (x : xs) newElem =
  if elemIndex < 0
    then x : xs
    else x : replace (elemIndex - 1) xs newElem

-- count the amount of children in a 3x3 submatrix given it's center
countChildrenAround :: Environment -> (Int, Int) -> Int
countChildrenAround env (r, c) =
  let childInCenter = getChildInCell env r c
   in ( if isJust childInCenter
          then 1
          else 0
      )
        + sum
          [ 1
            | direction <-
                [ DUp,
                  DRight,
                  DDown,
                  DLeft,
                  DDiagonalUpLeft,
                  DDiagonalUpRight,
                  DDiagonalDownLeft,
                  DDiagonalDownRight
                ],
              let newCell = adjacentCell env (r, c) direction
                  (newR, newC) = fromMaybe (-1, -1) newCell
               in isJust newCell
                    && isJust (getChildInCell env newR newC)
          ]

-- Unique elements of an iterable
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

-- Select n elements randomly from an iterable
randomSelect :: Int -> [a] -> StdGen -> [a]
randomSelect n list gen =
  -- Return n elements randomly from list using gen as generator
  let x = take n $ unique (randomRs (0, length list - 1) gen)
   in map (list !!) x

-- Simulate take elements from iterable with given select probability
_randomlyTake :: [a] -> [a] -> Double -> StdGen -> ([a], StdGen)
_randomlyTake curList [] _ gen = (curList, gen)
_randomlyTake curList (x : xs) prob gen =
  let (r, newGen) = randomR (0, 1) gen :: (Double, StdGen)
   in if r <= prob
        then _randomlyTake (x : curList) xs prob newGen
        else _randomlyTake curList xs prob newGen

randomlyTake :: [a] -> Double -> StdGen -> ([a], StdGen)
randomlyTake = _randomlyTake []

-- Check if an element exists in an iterable
exists :: (Eq a) => a -> [a] -> Bool
exists _ [] = False
exists e (x : xs) = (e == x) || exists e xs

-- Check whether the child is loaded by a robot given it's child index
childIsLoaded :: Environment -> Int -> Bool
childIsLoaded Environment {robots = robots} index =
  exists (Just index) (map loadingChild robots)

-- Count the amount of children in (r, c)
childrenAmountInPosition :: Environment -> (Int, Int) -> Int
childrenAmountInPosition env (r, c) =
  length $ filter (\(Child x y) -> (x, y) == (r, c)) (children env)

-- Count the amount of dirty in the Environment
dirtAmount :: Environment -> Int
dirtAmount env = length (dirt env)

-- Count the amount of empty cells in the Environment
emptyCells :: Environment -> Int
emptyCells env =
  length [1 | r <- [0 .. n env - 1], c <- [0 .. m env - 1], isCellFree (r, c) env]

-- Get the percent of free cells in the Environment
freeCellsPercent :: Environment -> Double
freeCellsPercent env =
  empty / (empty + dirty)
  where
    empty = fromIntegral $ emptyCells env :: Double
    dirty = fromIntegral $ dirtAmount env :: Double

-- Get the available direction from a position
getAvailableDirections :: Environment -> (Int, Int) -> [Direction]
getAvailableDirections env pos =
  [ direction
    | direction <- [DUp, DRight, DDown, DLeft],
      let adj = adjacentCell env pos direction
       in isJust adj && canMoveRobotInPosition env pos direction
  ]

-- Get the available neighbours cells in Environment from a position, if neighbour is not available return Nothing
-- Order is [DUp, DRight, DDown, DLeft]
getNeighbours :: Environment -> (Int, Int) -> [(Maybe (Int, Int), Direction)]
getNeighbours env pos =
  [(adjacentCell env pos direction, direction) | direction <- getAvailableDirections env pos]
