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
    countChildrenAround,
    getChildrenPositions,
    getCorralsPositions,
    getDirtPositions,
    getObstacleInCell,
    getRandomCellsInSquare,
    getRandomCellsInSquareNotContaining,
    getRobotsPositions,
    isCellFree,
    isCellInRange,
    randomSelect,
    randomlyTake,
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

genInitialChildren :: Int -> Int -> Int -> [(Int, Int)] -> Int -> [Child]
genInitialChildren n m childrenAmount notContainingCells seed =
  [ Child x y
    | (x, y) <- getRandomCellsInSquareNotContaining n m childrenAmount seed notContainingCells
  ]

genInitialDirt :: Int -> Int -> Int -> [(Int, Int)] -> Int -> [Dirt]
genInitialDirt n m dirtAmount notContainingCells seed =
  [ Dirt x y
    | (x, y) <- getRandomCellsInSquareNotContaining n m dirtAmount seed notContainingCells
  ]

genInitialCorrals :: Int -> Int -> Int -> [(Int, Int)] -> Int -> [Corral]
genInitialCorrals n m corralsAmount notContainingCells seed =
  [ Corral x y
    | (x, y) <- getRandomCellsInSquareNotContaining n m corralsAmount seed notContainingCells
  ]

genInitialObstacles :: Int -> Int -> Int -> [(Int, Int)] -> Int -> [Obstacle]
genInitialObstacles n m obstaclesAmount notContainingCells seed =
  [ Obstacle x y
    | (x, y) <- getRandomCellsInSquareNotContaining n m obstaclesAmount seed notContainingCells
  ]

genInitialState :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Environment
genInitialState n m childrenAmount robotsAmount obstaclesAmount dirtAmount seed =
  let robots = genInitialRobots n m robotsAmount seed
      robotsPositions = getRobotsPositions robots

      children = genInitialChildren n m childrenAmount robotsPositions seed
      childrenPositions = getChildrenPositions children

      dirt = genInitialDirt n m dirtAmount (robotsPositions ++ childrenPositions) seed
      dirtPositions = getDirtPositions dirt

      corrals =
        genInitialCorrals
          n
          m
          childrenAmount
          (robotsPositions ++ childrenPositions ++ dirtPositions)
          seed
      corralsPositions = getCorralsPositions corrals

      obstacles =
        genInitialObstacles
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

-- Dirt functions

-- Get the amount dirt to generate based on children amount
amountDirtToGenerate :: Int -> Int
amountDirtToGenerate childrenAround
  | childrenAround == 1 = 1
  | childrenAround == 2 = 3
  | childrenAround > 2 = 6
  | otherwise = 0

-- Gen Dirt in environment around center (r, c)
genDirt :: Environment -> (Int, Int) -> StdGen -> (Environment, StdGen)
genDirt env (r, c) gen =
  let childrenAround = countChildrenAround env (r, c)
      dirtAvailablePositions =
        [ (x, y)
          | x <- [r - 1 .. r + 1],
            y <- [c - 1 .. c + 1],
            isCellInRange x y env,
            isCellFree (x, y) env
        ]
      maxAmountDirtToGenerate = amountDirtToGenerate childrenAround
      candidatePositions =
        randomSelect
          (min maxAmountDirtToGenerate (length dirtAvailablePositions))
          dirtAvailablePositions
          gen
      (selectedPositions, newGen) = randomlyTake candidatePositions 0.5 gen
      newDirt = dirt env ++ [Dirt x y | (x, y) <- selectedPositions]
   in (env {dirt = newDirt}, newGen)

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
moveChild :: (Environment, Int) -> ChildAction -> StdGen -> (Environment, StdGen)
moveChild (env, _) CStay gen = (env, gen)
moveChild (env, index) action gen
  | not $ canMoveChild (env, index) (fromJust $ childActionToDirection action) = (env, gen)
  | otherwise =
    let child = children env !! index
        childPos = (\(Child x y) -> (x, y)) child
        direction = fromJust $ childActionToDirection action
        (newR, newC) = fromJust $ adjacentCell env childPos direction
        newChild = Child newR newC
        envAfterMoveChild = env {children = replace index (children env) newChild}
        obstacle = getObstacleInCell env newR newC
        obstacleIndex =
          if isNothing obstacle
            then Nothing
            else findIndex (\o -> o == fromJust obstacle) (obstacles env)
        envAfterMoveObstacles =
          if isNothing obstacleIndex
            then envAfterMoveChild
            else moveObstacle (envAfterMoveChild, fromJust obstacleIndex) direction
        (envAfterGenDirt, newGen) = genDirt envAfterMoveObstacles childPos gen
     in (envAfterGenDirt, newGen)

moveChildRandomly :: Environment -> Int -> StdGen -> (Environment, StdGen)
moveChildRandomly env index gen =
  let child = children env !! index
      (action, newGen) = randomChildAction env child gen
      (newEnv, newGen2) = moveChild (env, index) action newGen
   in (newEnv, newGen2)

_moveChildrenRandomly :: Environment -> Int -> StdGen -> (Environment, StdGen)
_moveChildrenRandomly env cur gen =
  if cur == childrenAmount env
    then (env, gen)
    else
      let (newEnv, newGen) = moveChildRandomly env cur gen
       in _moveChildrenRandomly newEnv (cur + 1) newGen

moveChildrenRandomly :: Environment -> StdGen -> (Environment, StdGen)
moveChildrenRandomly env = _moveChildrenRandomly env 0
