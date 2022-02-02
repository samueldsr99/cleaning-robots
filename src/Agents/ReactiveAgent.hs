module Agents.ReactiveAgent where

import Agents.Utils
  ( BfsState (BfsState),
    anyDirtyCell,
    anyKidOutSideCorral,
    bestChildToLoad,
    bfs,
    distanceToNearbyCorral,
    isCorral,
    isDirty,
    isFreeCorral,
    isLoadingChild,
    nextDirectionToBestChildToLoad,
    nextDirectionToNearbyEmptyCorral,
    nextDirectionToNearestDirtyCell,
    reachableChildrenStates,
  )
import Data.Foldable (find)
import Data.List (nub, sortOn)
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import Env (genInitialState)
import System.Random (StdGen, mkStdGen)
import Types
  ( Child (Child),
    Corral (Corral),
    Direction (..),
    Dirt (Dirt),
    Environment (..),
    Obstacle (Obstacle),
    Robot (..),
    RobotAction (..),
  )
import UI (printEnvironment)
import Utils (adjacentCell, canMoveRobotInPosition, childrenAmountInPosition, getCorralInCell, getDirtInCell, getRobotInCell, robotDirectionToAction)

-- Action method
getAction :: (Environment, Int) -> StdGen -> (RobotAction, StdGen)
getAction (env, index) gen
  | isLoadingChild robot && isCorral env robotPosition && (childrenAmountInPosition env robotPosition == 1) = (RDropChild, gen)
  | isLoadingChild robot && isDirty env robotPosition = (RClean, gen)
  | isLoadingChild robot =
    let direction = nextDirectionToNearbyEmptyCorral env (position robot)
        action =
          if isJust direction
            then fromJust $ robotDirectionToAction (fromJust direction)
            else RStay
     in (action, gen)
  | not (isLoadingChild robot) && not (anyKidOutSideCorral env) && isDirty env robotPosition = (RClean, gen)
  | not (isLoadingChild robot) && not (anyKidOutSideCorral env) && anyDirtyCell env =
    let direction = nextDirectionToNearestDirtyCell env (position robot)
        action =
          if isJust direction
            then fromJust $ robotDirectionToAction (fromJust direction)
            else RStay
     in (action, gen)
  | not (isLoadingChild robot) && anyKidOutSideCorral env =
    let direction = nextDirectionToBestChildToLoad env (position robot)
        action =
          if isJust direction
            then fromJust $ robotDirectionToAction (fromJust direction)
            else RStay
     in (action, gen)
  | otherwise = (RStay, gen)
  where
    robot = robots env !! index
    robotPosition = position robot

test :: IO ()
test =
  let robot1 = Robot {idx = 1, rtype = "A", position = (1, 3), loadingChild = Just 0}
      robot2 = Robot {idx = 2, rtype = "A", position = (0, 4), loadingChild = Nothing}
      env = Environment {n = 10, m = 10, robotsAmount = 2, childrenAmount = 5, robots = [Robot {idx = 1, rtype = "A", position = (0, 7), loadingChild = Nothing}, Robot {idx = 2, rtype = "A", position = (5, 3), loadingChild = Just 2}], children = [Child 6 7, Child 0 8, Child 5 3, Child 1 6, Child 9 1], dirt = [Dirt 1 5, Dirt 5 8, Dirt 1 4, Dirt 2 6, Dirt 3 6, Dirt 3 4, Dirt 8 1, Dirt 4 7, Dirt 2 3, Dirt 9 0, Dirt 2 2, Dirt 2 1, Dirt 1 9, Dirt 3 1, Dirt 9 3, Dirt 4 1, Dirt 6 2, Dirt 5 3, Dirt 6 0, Dirt 6 1, Dirt 6 3, Dirt 8 0, Dirt 7 1, Dirt 7 2, Dirt 8 2], corrals = [Corral 7 3, Corral 0 8, Corral 6 7, Corral 1 6, Corral 9 6], obstacles = [Obstacle 6 5, Obstacle 7 7, Obstacle 0 9, Obstacle 7 0, Obstacle 1 8]}
   in do
        printEnvironment env
        print $ getAction (env, 0) (mkStdGen 40)
        print $ nextDirectionToBestChildToLoad env (0, 7)
