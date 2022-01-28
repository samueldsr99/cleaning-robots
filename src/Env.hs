-- Module for handling States & Transitions
module Env where

import Control.Monad (replicateM)
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
    childActionToDirection,
    childCanVisitCell,
    getChildrenPositions,
    getCorralsPositions,
    getDirtPositions,
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

-- Available actions for a child in the environment
childActions :: Environment -> Child -> [ChildAction]
childActions env (Child r c) =
  [ x
    | x <- [CUp, CRight, CDown, CLeft],
      let pos = adjacentCell env (r, c) (fromJust $ childActionToDirection x)
       in isJust pos && childCanVisitCell (fromJust pos) env
  ]

randomChildAction :: Environment -> Child -> StdGen -> (ChildAction, StdGen)
randomChildAction env child gen =
  let actions = CStay : childActions env child
      (r, newGen) = randomR (0, length actions - 1) gen :: (Int, StdGen)
   in (actions !! r, newGen)

moveChildRandomly :: Environment -> Int -> StdGen -> (Environment, StdGen)
moveChildRandomly env index gen =
  let child = children env !! index
      (action, newGen) = randomChildAction env child gen
   in if action == CStay
        then (env, newGen)
        else
          let direction = fromJust $ childActionToDirection action
              childPos = (\(Child x y) -> (x, y)) child
              (newR, newC) = fromJust $ adjacentCell env childPos direction
              newChild = Child newR newC
           in (env {children = replace index (children env) newChild}, newGen)

_moveChildrenRandomly :: Environment -> Int -> StdGen -> (Environment, StdGen)
_moveChildrenRandomly env cur gen =
  if cur == childrenAmount env
    then (env, gen)
    else
      let (newEnv, newGen) = moveChildRandomly env cur gen
       in _moveChildrenRandomly newEnv (cur + 1) newGen

moveChildrenRandomly :: Environment -> StdGen -> (Environment, StdGen)
moveChildrenRandomly env = _moveChildrenRandomly env 0
