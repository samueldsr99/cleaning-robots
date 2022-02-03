module Agents.Utils where

import qualified Data.Bifunctor
import Data.List (elemIndex, find, findIndex, nub, sortOn, (\\))
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing)
import Debug.Trace (trace)
import Types
  ( Child (..),
    Direction (..),
    Dirt (Dirt),
    Environment (..),
    RObjective (..),
    Robot (..),
    RobotAction (..),
  )
import Utils (adjacentCell, canMoveRobot, canMoveRobotInPosition, getAvailableDirections, getChildInCell, getCorralInCell, getDirtInCell, getNeighbours, replace)

-- Rules

-- Check if robot is loading a child
isLoadingChild :: Robot -> Bool
isLoadingChild robot = isJust $ loadingChild robot

-- Check if current cell is a corral
isCorral :: Environment -> (Int, Int) -> Bool
isCorral env (r, c) = isJust $ getCorralInCell env r c

isFreeCorral :: Environment -> (Int, Int) -> Bool
isFreeCorral env (r, c) =
  isJust (getCorralInCell env r c) && isNothing (getChildInCell env r c)

-- Check if current cell is dirty
isDirty :: Environment -> (Int, Int) -> Bool
isDirty env (r, c) = isJust $ getDirtInCell env r c

-- Check whether exists a kid outside a corral
anyKidOutSideCorral :: Environment -> Bool
anyKidOutSideCorral env =
  any
    ( \(Child r c) ->
        isNothing (getCorralInCell env r c)
          && (r, c) `notElem` getObjectiveRelatedPositions env ["loadChild", "dropChild"]
    )
    (children env)

-- Check whether exists a dirty cell
anyDirtyCell :: Environment -> Bool
anyDirtyCell env =
  any
    ( \(Dirt r c) ->
        (r, c) `notElem` getObjectiveDirtyPositions env
    )
    (dirt env)

-- Some other utils functions

data BfsState = BfsState (Int, Int) Int deriving (Show, Eq)

-- Traverse de environment as a robot and return each reachable position with
-- their respective distance from starting point
bfs :: Environment -> [BfsState] -> [BfsState] -> [BfsState]
bfs _ visited [] = visited
bfs env visited ((BfsState curPosition curDistance) : queue) = bfs env visited' queue'
  where
    cur = BfsState curPosition curDistance
    neighbours =
      [ let newDistance = curDistance + 1
            newPosition = fromJust $ adjacentCell env curPosition direction
         in BfsState newPosition newDistance
        | direction <- [DUp, DRight, DDown, DLeft],
          let adj = adjacentCell env curPosition direction
              visitedPositions = map (\(BfsState position_ _) -> position_) visited
           in isJust adj
                && canMoveRobotInPosition env curPosition direction
                && notElem (fromJust adj) visitedPositions
      ]
    visited' = visited ++ neighbours
    queue' = queue ++ neighbours

-- Get the bfs states of all children reachable from (r, c) walking as a robot
reachableChildrenStates :: Environment -> (Int, Int) -> [(Int, Int)] -> [BfsState]
reachableChildrenStates env (r, c) excludes =
  let states = bfs env [BfsState (r, c) 0] [BfsState (r, c) 0]
      reachableChildrenStates =
        filter
          ( \(BfsState (r, c) _) ->
              isJust (getChildInCell env r c)
                && isNothing (getCorralInCell env r c)
                && (r, c) `notElem` excludes
          )
          states
   in reachableChildrenStates

-- Get the distance to the nearby empty corral from (r, c)
distanceToNearbyCorral :: Environment -> (Int, Int) -> Int
distanceToNearbyCorral env (r, c) =
  let states = bfs env [BfsState (r, c) 0] [BfsState (r, c) 0]
      freeCorralStates = filter (\(BfsState (r, c) _) -> isFreeCorral env (r, c)) states
      minDistanceCorral =
        if not (null freeCorralStates)
          then Just $ head $ sortOn (\(BfsState _ distance) -> distance) freeCorralStates
          else Nothing
      minDistance =
        if isNothing minDistanceCorral
          then 9999999
          else let BfsState _ distance = fromJust minDistanceCorral in distance
   in minDistance

-- Get the best child to load: Minimizes distance from robot to child + distance from child to an empty corral
bestChildToLoad :: Environment -> [BfsState] -> Maybe BfsState
bestChildToLoad _ [] = Nothing
bestChildToLoad env childrenPositions =
  let childValue = (\(BfsState pos distanceToRobot) -> distanceToRobot + distanceToNearbyCorral env pos)
   in Just $ head $ sortOn childValue childrenPositions

-- Get the nearest dirty cell starting at (r, c)
nearestDirtyCell :: Environment -> (Int, Int) -> [(Int, Int)] -> Maybe BfsState
nearestDirtyCell env curPos excludes =
  let states = bfs env [BfsState curPos 0] [BfsState curPos 0]
      dirtyCells = filter (\(BfsState (r, c) _) -> isJust $ getDirtInCell env r c) states
      dirtyCellsExcluded = filter (\(BfsState pos _) -> pos `notElem` excludes) dirtyCells
   in if not (null dirtyCellsExcluded)
        then Just $ head $ sortOn (\(BfsState pos distance) -> distance) dirtyCellsExcluded
        else Nothing

getObjectiveRelatedPositions :: Environment -> [String] -> [(Int, Int)]
getObjectiveRelatedPositions env topics =
  let objectives = map (\Robot {objective = _objective} -> _objective) (robots env)
      childrenRelatedObjectives =
        catMaybes $
          filter
            ( \o ->
                isJust o && (otype (fromJust o) `elem` topics)
            )
            objectives
   in map (\(RObjective _ pos) -> pos) childrenRelatedObjectives

getObjectiveChildrenPositions :: Environment -> [(Int, Int)]
getObjectiveChildrenPositions env = getObjectiveRelatedPositions env ["loadChild"]

getObjectiveDirtyPositions :: Environment -> [(Int, Int)]
getObjectiveDirtyPositions env = getObjectiveRelatedPositions env ["cleanDirt"]

getObjectiveCorralPositions :: Environment -> [(Int, Int)]
getObjectiveCorralPositions env = getObjectiveRelatedPositions env ["dropChild"]

updateRobotObjective :: (Environment, Int) -> Maybe RObjective -> Environment
updateRobotObjective (env, index) objective_ =
  let robot = robots env !! index
      newRobot = robot {objective = objective_}
   in env {robots = replace index (robots env) newRobot}

cleanRobotObjective :: (Environment, Int) -> Environment
cleanRobotObjective (env, index) = updateRobotObjective (env, index) Nothing
