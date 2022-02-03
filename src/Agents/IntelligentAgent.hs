module Agents.IntelligentAgent where

import Agents.ReactiveAgent (getAction, nextDirectionToBestChildToLoad, nextDirectionToNearbyEmptyCorral, nextDirectionToNearestDirtyCell)
import Agents.Utils (BfsState (BfsState), anyDirtyCell, anyKidOutSideCorral, bestChildToLoad, cleanRobotObjective, distanceToNearbyCorral, getObjectiveChildrenPositions, getObjectiveCorralPositions, getObjectiveDirtyPositions, isCorral, isDirty, isLoadingChild, nearestDirtyCell, reachableChildrenStates, updateRobotObjective)
import Data.Maybe (catMaybes, fromJust, isJust)
import System.Random (StdGen, mkStdGen)
import Types
  ( Child (Child),
    Corral (Corral),
    Dirt (Dirt),
    Environment (..),
    Obstacle (Obstacle),
    RObjective (RObjective, opos, otype),
    Robot (..),
    RobotAction (..),
  )
import UI
import Utils (adjacentCell, childrenAmountInPosition, replace, robotDirectionToAction)

getAction_ :: (Environment, Int) -> StdGen -> (Environment, RobotAction, StdGen)
getAction_ (env, index) gen
  | isLoadingChild robot && isCorral env robotPosition && (childrenAmountInPosition env robotPosition == 1) =
    (cleanRobotObjective (env, index), RDropChild, gen)
  | isLoadingChild robot && isDirty env robotPosition =
    (updateRobotObjective (env, index) (Just RObjective {otype = "cleanDirt", opos = robotPosition}), RClean, gen)
  | isLoadingChild robot =
    let newEnv = cleanRobotObjective (env, index)
        excludes = getObjectiveCorralPositions newEnv
        direction = nextDirectionToNearbyEmptyCorral newEnv (position robot) excludes
        action =
          if isJust direction
            then fromJust $ robotDirectionToAction (fromJust direction)
            else RStay
        cell = fromJust $ adjacentCell env robotPosition (fromJust direction)
        newObjective = Just RObjective {otype = "dropChild", opos = cell}
     in (updateRobotObjective (env, index) newObjective, action, gen)
  | not (isLoadingChild robot) && not (anyKidOutSideCorral env) && isDirty env robotPosition =
    (cleanRobotObjective (env, index), RClean, gen)
  | not (isLoadingChild robot) && not (anyKidOutSideCorral env) && anyDirtyCell env =
    let newEnv = cleanRobotObjective (env, index)
        excludes = getObjectiveDirtyPositions newEnv
        cell = nearestDirtyCell newEnv (position robot) excludes
        direction = nextDirectionToNearestDirtyCell newEnv (position robot) excludes
        action =
          if isJust direction
            then fromJust $ robotDirectionToAction (fromJust direction)
            else RStay
        newObjective =
          if isJust cell
            then
              let BfsState objPos _ = fromJust cell
               in Just RObjective {otype = "cleanDirt", opos = objPos}
            else Nothing
     in (updateRobotObjective (newEnv, index) newObjective, action, gen)
  | not (isLoadingChild robot) && anyKidOutSideCorral env =
    let newEnv = cleanRobotObjective (env, index)
        excludes = getObjectiveChildrenPositions newEnv
        reachableChildrens = reachableChildrenStates newEnv robotPosition excludes
        cell = bestChildToLoad newEnv reachableChildrens
        direction = nextDirectionToBestChildToLoad newEnv (position robot)
        action =
          if isJust direction
            then fromJust $ robotDirectionToAction (fromJust direction)
            else RStay
        newObjective =
          if isJust cell
            then
              let BfsState objPos _ = fromJust cell
               in Just RObjective {otype = "loadChild", opos = objPos}
            else Nothing
     in (updateRobotObjective (newEnv, index) newObjective, action, gen)
  | otherwise = (cleanRobotObjective (env, index), RStay, gen)
  where
    robot = robots env !! index
    robotPosition = position robot

getAction :: (Environment, Int) -> StdGen -> (Environment, RobotAction, StdGen)
getAction (env, index) = getAction_ (cleanRobotObjective (env, index), index)
