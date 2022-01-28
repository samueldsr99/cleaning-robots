-- Module for handling States & Transitions
module Env where

import Control.Monad (replicateM)
import Data.List (findIndex)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import System.Random (Random (randomR), StdGen, mkStdGen)
import Types
  ( Child (Child),
    ChildAction (..),
    Corral (Corral),
    Direction (..),
    Dirt (Dirt),
    Environment (..),
    Obstacle (Obstacle),
    RType,
    Robot (..),
  )
import Utils
  ( adjacentCell,
    canMoveChild,
    canMoveObstacle,
    childActionToDirection,
    getChildrenPositions,
    getCorralsPositions,
    getDirtPositions,
    getObstacleInCell,
    getRandomCellsInSquare,
    getRandomCellsInSquareNotContaining,
    getRobotsPositions,
    isCellFree,
    replace,
  )

-- Initial State generations

genInitialRobots :: Int -> Int -> Int -> Int -> [Robot]
genInitialRobots n m robotsAmount seed =
  let robotPositions = getRandomCellsInSquare n m robotsAmount seed
      robots =
        [ Robot
            { idx = idx,
              rtype = "A",
              position = pos,
              loadingChild = False
            }
          | (pos, idx) <- zip robotPositions [1 .. robotsAmount]
        ]
   in robots

genChildren :: Int -> Int -> Int -> [(Int, Int)] -> Int -> [Child]
genChildren n m childrenAmount notContainingCells seed =
  [ Child x y
    | (x, y) <- getRandomCellsInSquareNotContaining n m childrenAmount seed notContainingCells
  ]

genDirt :: Int -> Int -> Int -> [(Int, Int)] -> Int -> [Dirt]
genDirt n m dirtAmount notContainingCells seed =
  [ Dirt x y
    | (x, y) <- getRandomCellsInSquareNotContaining n m dirtAmount seed notContainingCells
  ]

genCorrals :: Int -> Int -> Int -> [(Int, Int)] -> Int -> [Corral]
genCorrals n m corralsAmount notContainingCells seed =
  [ Corral x y
    | (x, y) <- getRandomCellsInSquareNotContaining n m corralsAmount seed notContainingCells
  ]

genObstacles :: Int -> Int -> Int -> [(Int, Int)] -> Int -> [Obstacle]
genObstacles n m obstaclesAmount notContainingCells seed =
  [ Obstacle x y
    | (x, y) <- getRandomCellsInSquareNotContaining n m obstaclesAmount seed notContainingCells
  ]

genInitialState :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Environment
genInitialState n m childrenAmount robotsAmount obstaclesAmount dirtAmount seed =
  let robots = genInitialRobots n m robotsAmount seed
      robotsPositions = getRobotsPositions robots

      children = genChildren n m childrenAmount robotsPositions seed
      childrenPositions = getChildrenPositions children

      dirt = genDirt n m dirtAmount (robotsPositions ++ childrenPositions) seed
      dirtPositions = getDirtPositions dirt

      corrals =
        genCorrals
          n
          m
          childrenAmount
          (robotsPositions ++ childrenPositions ++ dirtPositions)
          seed
      corralsPositions = getCorralsPositions corrals

      obstacles =
        genObstacles
          n
          m
          obstaclesAmount
          (robotsPositions ++ childrenPositions ++ dirtPositions ++ corralsPositions)
          seed
   in Environment
        { n = n,
          m = m,
          robotsAmount = robotsAmount,
          childrenAmount = childrenAmount,
          robots = robots,
          children = children,
          dirt = dirt,
          corrals = corrals,
          obstacles = obstacles
        }

-- Obstacle functions

-- Move obstacle in a direction given the obstacle index
moveObstacle :: (Environment, Int) -> Direction -> Environment
moveObstacle (env, index) direction
  | not $ canMoveObstacle (env, index) direction = env
  | otherwise =
    let obstacle = obstacles env !! index
        obstaclePosition = (\(Obstacle r c) -> (r, c)) obstacle
        (newR, newC) = fromJust $ adjacentCell env obstaclePosition direction
        newObstacle = Obstacle newR newC
        newEnv = env {obstacles = replace index (obstacles env) newObstacle}
        obstacleIndexInCell = findIndex (\(Obstacle oR oC) -> (oR, oC) == (newR, newC)) (obstacles env)
     in if isNothing obstacleIndexInCell
          then newEnv
          else moveObstacle (newEnv, fromJust obstacleIndexInCell) direction

-- Child functions

-- Available actions for a child in the environment
childActions :: Environment -> Child -> [ChildAction]
childActions env (Child r c) =
  [ x
    | x <- [CUp, CRight, CDown, CLeft],
      let direction = fromJust $ childActionToDirection x
          pos = adjacentCell env (r, c) direction
          childIndex = fromJust $ findIndex (\(Child x y) -> (x, y) == (r, c)) (children env)
       in isJust pos && canMoveChild (env, childIndex) direction
  ]

randomChildAction :: Environment -> Child -> StdGen -> (ChildAction, StdGen)
randomChildAction env child gen =
  let actions = CStay : childActions env child
      (r, newGen) = randomR (0, length actions - 1) gen :: (Int, StdGen)
   in (actions !! r, newGen)

-- Move a child on a direction given the child index and return the new Environment
moveChild :: (Environment, Int) -> ChildAction -> Environment
moveChild (env, index) CStay = env
moveChild (env, index) action
  | not $ canMoveChild (env, index) (fromJust $ childActionToDirection action) = env
  | otherwise =
    let child = children env !! index
        direction = fromJust $ childActionToDirection action
        childPos = (\(Child x y) -> (x, y)) child
        (newR, newC) = fromJust $ adjacentCell env childPos direction
        newChild = Child newR newC
        newEnv = env {children = replace index (children env) newChild}
        obstacle = getObstacleInCell env newR newC
        obstacleIndex =
          if isNothing obstacle
            then Nothing
            else findIndex (\o -> o == fromJust obstacle) (obstacles env)
     in -- Move obstacles
        if isNothing obstacleIndex
          then newEnv
          else moveObstacle (newEnv, fromJust obstacleIndex) direction

moveChildRandomly :: Environment -> Int -> StdGen -> (Environment, StdGen)
moveChildRandomly env index gen =
  let child = children env !! index
      (action, newGen) = randomChildAction env child gen
      newEnv = moveChild (env, index) action
   in (newEnv, newGen)

_moveChildrenRandomly :: Environment -> Int -> StdGen -> (Environment, StdGen)
_moveChildrenRandomly env cur gen =
  if cur == childrenAmount env
    then (env, gen)
    else
      let (newEnv, newGen) = moveChildRandomly env cur gen
       in _moveChildrenRandomly newEnv (cur + 1) newGen

moveChildrenRandomly :: Environment -> StdGen -> (Environment, StdGen)
moveChildrenRandomly env = _moveChildrenRandomly env 0
