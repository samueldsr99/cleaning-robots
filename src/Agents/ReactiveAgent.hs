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
    isLoadingChild,
    nextDirectionToBestChildToLoad,
    nextDirectionToNearbyEmptyCorral,
    nextDirectionToNearestDirtyCell,
    reachableChildrenStates,
  )
import Data.Foldable (find)
import Data.List (nub, sortOn)
import Data.Maybe (fromJust, isJust, isNothing)
import Env (genInitialState)
import System.Random (StdGen)
import Types
  ( Child (Child),
    Corral (Corral),
    Dirt (Dirt),
    Environment (..),
    Obstacle (Obstacle),
    Robot (..),
    RobotAction (..),
  )
import UI (printEnvironment)
import Utils (getCorralInCell, getDirtInCell, getRobotInCell, robotDirectionToAction)

-- Action method
getAction :: (Environment, Int) -> StdGen -> (RobotAction, StdGen)
getAction (env, index) gen
  | isLoadingChild robot && isCorral env robotPosition = (RDropChild, gen)
  | isLoadingChild robot && isDirty env robotPosition = (RClean, gen)
  | isLoadingChild robot =
    let action = fromJust $ robotDirectionToAction (nextDirectionToNearbyEmptyCorral env (position robot))
     in (action, gen)
  | not (isLoadingChild robot) && not (anyKidOutSideCorral env) && isDirty env robotPosition = (RClean, gen)
  | not (isLoadingChild robot) && not (anyKidOutSideCorral env) && anyDirtyCell env =
    let action = fromJust $ robotDirectionToAction (nextDirectionToNearestDirtyCell env (position robot))
     in (action, gen)
  | not (isLoadingChild robot) && anyKidOutSideCorral env =
    let action = fromJust $ robotDirectionToAction (nextDirectionToBestChildToLoad env (position robot))
     in (action, gen)
  | otherwise = (RStay, gen)
  where
    robot = robots env !! index
    robotPosition = position robot

test :: IO ()
test =
  let env = genInitialState 5 5 2 1 3 1 98
      robot = Robot {idx = 1, rtype = "A", position = (1, 4), loadingChild = Nothing}
      env2 = Environment {n = 5, m = 5, robotsAmount = 0, childrenAmount = 2, robots = [], children = [Child 1 2, Child 2 3], dirt = [Dirt 0 0], corrals = [Corral 1 2, Corral 2 3], obstacles = []}
   in do
        printEnvironment env2
        print $ any (\(Child r c) -> isNothing $ getCorralInCell env2 r c) (children env2)
        print $ anyKidOutSideCorral env2
