-- Module for handling States & Transitions
module Env (genInitialState) where

import Control.Monad (replicateM)
import Types
  ( Child (Child),
    Corral (Corral),
    Dirt (Dirt),
    Environment (..),
    Obstacle (Obstacle),
    RType,
    Robot (..),
  )
import Utils
  ( getChildrenPositions,
    getCorralsPositions,
    getDirtPositions,
    getRandomCellsInSquare,
    getRandomCellsInSquareNotContaining,
    getRobotsPositions,
  )

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
