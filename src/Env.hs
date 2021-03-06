-- Module for handling States & Transitions
module Env where

import Control.Monad (replicateM)
import Data.List (delete, elemIndex, findIndex)
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
    Robot (..),
    RobotAction (..),
  )
import Utils
  ( adjacentCell,
    canMoveChild,
    canMoveObstacle,
    canMoveRobot,
    childActionToDirection,
    countChildrenAround,
    getChildInCell,
    getChildrenPositions,
    getCorralsPositions,
    getDirtInCell,
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
    robotActionToDirection,
  )

-- Initial State generations

genInitialRobots :: Int -> Int -> Int -> String -> StdGen -> ([Robot], StdGen)
genInitialRobots n m robotsAmount agentsType gen =
  let (robotPositions, newGen) = getRandomCellsInSquare n m robotsAmount gen
      robots =
        [ Robot
            { idx = idx,
              rtype = agentsType,
              position = pos,
              loadingChild = Nothing,
              objective = Nothing
            }
          | (pos, idx) <- zip robotPositions [1 .. robotsAmount]
        ]
   in (robots, newGen)

genInitialChildren :: Int -> Int -> Int -> [(Int, Int)] -> StdGen -> ([Child], StdGen)
genInitialChildren n m childrenAmount notContainingCells gen =
  let (randomCells, newGen) = getRandomCellsInSquareNotContaining n m childrenAmount gen notContainingCells
   in ( [ Child x y
          | (x, y) <- randomCells
        ],
        gen
      )

genInitialDirt :: Int -> Int -> Int -> [(Int, Int)] -> StdGen -> ([Dirt], StdGen)
genInitialDirt n m dirtAmount notContainingCells gen =
  let (randomCells, newGen) = getRandomCellsInSquareNotContaining n m dirtAmount gen notContainingCells
   in ( [ Dirt x y
          | (x, y) <- randomCells
        ],
        newGen
      )

genInitialCorrals :: Int -> Int -> Int -> [(Int, Int)] -> StdGen -> ([Corral], StdGen)
genInitialCorrals n m corralsAmount notContainingCells gen =
  let (randomCells, newGen) = getRandomCellsInSquareNotContaining n m corralsAmount gen notContainingCells
   in ( [ Corral x y
          | (x, y) <- randomCells
        ],
        newGen
      )

genInitialObstacles :: Int -> Int -> Int -> [(Int, Int)] -> StdGen -> ([Obstacle], StdGen)
genInitialObstacles n m obstaclesAmount notContainingCells gen =
  let (randomCells, newGen) = getRandomCellsInSquareNotContaining n m obstaclesAmount gen notContainingCells
   in ( [ Obstacle x y
          | (x, y) <- randomCells
        ],
        newGen
      )

genInitialState :: Int -> Int -> Int -> Int -> Int -> Int -> String -> StdGen -> (Environment, StdGen)
genInitialState n m childrenAmount robotsAmount obstaclesAmount dirtAmount agentsType  gen =
  let (robots, newGen) = genInitialRobots n m robotsAmount agentsType gen
      robotsPositions = getRobotsPositions robots

      (children, newGen2) = genInitialChildren n m childrenAmount robotsPositions newGen
      childrenPositions = getChildrenPositions children

      (dirt, newGen3) = genInitialDirt n m dirtAmount (robotsPositions ++ childrenPositions) newGen2
      dirtPositions = getDirtPositions dirt

      (corrals, newGen4) =
        genInitialCorrals
          n
          m
          childrenAmount
          (robotsPositions ++ childrenPositions ++ dirtPositions)
          newGen3
      corralsPositions = getCorralsPositions corrals

      (obstacles, newGen5) =
        genInitialObstacles
          n
          m
          obstaclesAmount
          (robotsPositions ++ childrenPositions ++ dirtPositions ++ corralsPositions)
          newGen4
   in ( Environment
          { n = n,
            m = m,
            robotsAmount = robotsAmount,
            childrenAmount = childrenAmount,
            robots = robots,
            children = children,
            dirt = dirt,
            corrals = corrals,
            obstacles = obstacles
          },
        newGen5
      )

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

-- Robot functions

_robotMoveActions :: Environment -> Int -> [RobotAction]
_robotMoveActions env index =
  let robot = robots env !! index
      pos = position robot
   in [ x
        | x <- [RUp, RDown, RLeft, RRight],
          let direction = fromJust $ robotActionToDirection x
              newPos = adjacentCell env (position robot) direction
           in isJust newPos && canMoveRobot (env, index) direction
      ]

_robotChildActions :: Environment -> Int -> [RobotAction]
_robotChildActions env index =
  let robot = robots env !! index
      (r, c) = position robot
      dirtInCell = getDirtInCell env r c
      obstacleInCell = getObstacleInCell env r c
   in [RDropChild | not (isJust dirtInCell || isJust obstacleInCell) && isJust (loadingChild robot)]

_robotCleaningActions :: Environment -> Int -> [RobotAction]
_robotCleaningActions env index =
  let robot = robots env !! index
      (r, c) = position robot
      dirtInCell = getDirtInCell env r c
   in [RClean | isJust dirtInCell]

-- Available actions for a robot in the environment
robotActions :: Environment -> Int -> [RobotAction]
robotActions env index =
  _robotMoveActions env index
    ++ _robotChildActions env index
    ++ _robotCleaningActions env index

-- Move robot in a direction
moveRobot :: (Environment, Int) -> Direction -> Environment
moveRobot (env, index) direction
  | not $ canMoveRobot (env, index) direction = env
  | otherwise =
    let robot = robots env !! index
        robotPosition = position robot
        -- Update robot coordinates
        (newRobotR, newRobotC) = fromJust $ adjacentCell env robotPosition direction
        -- If robot is carrying a child move child also
        childIndex = loadingChild robot
        updatedChildLoaded =
          if isJust childIndex
            then
              ( let child = children env !! fromJust childIndex
                    childPos = (\(Child x y) -> (x, y)) child
                    (newChildR, newChildC) = fromJust $ adjacentCell env childPos direction
                    newChild = Child newChildR newChildC
                 in Just newChild
              )
            else Nothing
        newEnvChildren =
          if isJust childIndex
            then replace (fromJust childIndex) (children env) (fromJust updatedChildLoaded)
            else children env
        -- If robot is in a child cell then carry him
        childInCell = getChildInCell env newRobotR newRobotC
        newChildLoaded =
          if isJust childInCell && isNothing (loadingChild robot)
            then elemIndex (fromJust childInCell) (children env)
            else loadingChild robot
        newRobot =
          robot
            { position = (newRobotR, newRobotC),
              loadingChild = newChildLoaded
            }
     in env
          { robots = replace index (robots env) newRobot,
            children = newEnvChildren
          }

-- Apply a robot action and return the new modified environment
applyRobotAction :: (Environment, Int) -> RobotAction -> Environment
applyRobotAction (env, index) action
  | action `notElem` robotActions env index || action == RStay = env
  | action `elem` [RUp, RDown, RLeft, RRight] =
    moveRobot (env, index) (fromJust $ robotActionToDirection action)
  | otherwise =
    let robot = robots env !! index
        (r, c) = position robot
     in if action == RDropChild
          then
            let newRobot = robot {loadingChild = Nothing}
             in env {robots = replace index (robots env) newRobot}
          else
            let dirtInCell = getDirtInCell env r c
                newDirtList =
                  if isJust dirtInCell
                    then delete (fromJust dirtInCell) (dirt env)
                    else dirt env
             in env {dirt = newDirtList}

-- Change the environment randomly creating new Env with the same parameters
shuffleEnv :: (Environment, StdGen) -> (Environment, StdGen)
shuffleEnv (env, gen) =
  let n_ = n env
      m_ = m env
      childrenAmount = length (children env)
      robotsAmount = length (robots env)
      agentsType = rtype $ head (robots env)
      dirtAmount = length (dirt env)
      obstaclesAmount = length (obstacles env)
      (newEnv, newGen) = genInitialState n_ m_ childrenAmount robotsAmount obstaclesAmount dirtAmount agentsType gen
   in (newEnv, newGen)
