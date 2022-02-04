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
    nearestDirtyCell,
    reachableChildrenStates,
  )
import qualified Control.Arrow as Data.Bifunctor
import Data.Foldable (find)
import Data.List (elemIndex, findIndex, nub, sortOn)
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
    RObjective (RObjective, opos, otype),
    Robot (..),
    RobotAction (..),
  )
import UI (printEnvironment)
import Utils (adjacentCell, canMoveRobotInPosition, childrenAmountInPosition, getAvailableDirections, getCorralInCell, getDirtInCell, getNeighbours, getRobotInCell, robotDirectionToAction)

-- Get the next direction to reach the best child to load starting at (r, c)
nextDirectionToBestChildToLoad :: Environment -> (Int, Int) -> Maybe Direction
nextDirectionToBestChildToLoad env (r, c) =
  let directions = getAvailableDirections env (r, c)
      neighbours = getNeighbours env (r, c)
      reachableChildrenFromNeighbours =
        [reachableChildrenStates env (fromJust pos) [] | (pos, _) <- neighbours]
      bestChildToLoadFromNeighbours =
        [bestChildToLoad env states | states <- reachableChildrenFromNeighbours]
      justBestChildToLoadFromNeighbours = catMaybes bestChildToLoadFromNeighbours
   in if null justBestChildToLoadFromNeighbours
        then Nothing
        else
          let bestChild = head $ sortOn (\(BfsState pos distance) -> distance) justBestChildToLoadFromNeighbours
              bestChildIndex = fromJust $ elemIndex (Just bestChild) bestChildToLoadFromNeighbours
           in Just $ directions !! bestChildIndex

-- Get the next direction in the path to reach the nearest dirty cell starting at (r, c)
nextDirectionToNearestDirtyCell :: Environment -> (Int, Int) -> [(Int, Int)] -> Maybe Direction
nextDirectionToNearestDirtyCell env (r, c) excludes =
  let neighbours = getNeighbours env (r, c)
      nearestDirtyCellFromNeighbours =
        map
          (Data.Bifunctor.first fromJust)
          ( filter
              (\(st, d) -> isJust st)
              [(nearestDirtyCell env (fromJust pos) excludes, direction) | (pos, direction) <- neighbours, isJust pos]
          )

      sortedCells = sortOn (\(BfsState _ distance, _) -> distance) nearestDirtyCellFromNeighbours
   in if null sortedCells
        then Nothing
        else Just $ snd $ head sortedCells

nextDirectionToNearbyEmptyCorral :: Environment -> (Int, Int) -> [(Int, Int)] -> Maybe Direction
nextDirectionToNearbyEmptyCorral env (r, c) excludes =
  let directions = getAvailableDirections env (r, c)
      neighbours = getNeighbours env (r, c)
      reachablePositionsFromNeighbours =
        [ bfs env [BfsState (fromJust pos) 0] [BfsState (fromJust pos) 0]
          | (pos, _) <- neighbours
        ]
      filterCorralPositions =
        filter
          ( \(BfsState (r, c) _) ->
              isFreeCorral env (r, c)
                && (r, c) `notElem` excludes
          )
      freeCorralPositionsFromNeighbours = map filterCorralPositions reachablePositionsFromNeighbours
      minFreeCorralPositionFromNeighbours =
        catMaybes
          [ if null x then Nothing else Just $ head (sortOn (\(BfsState _ distance) -> distance) x)
            | x <- freeCorralPositionsFromNeighbours
          ]
   in if null minFreeCorralPositionFromNeighbours
        then Nothing
        else
          let minDistanceCorral = head $ sortOn (\(BfsState _ distance) -> distance) minFreeCorralPositionFromNeighbours
              minDirectionIndex = fromJust $ findIndex (isJust . find (== minDistanceCorral)) freeCorralPositionsFromNeighbours
           in Just $ directions !! minDirectionIndex

-- Action method
getAction :: (Environment, Int) -> StdGen -> (Environment, RobotAction, StdGen)
getAction (env, index) gen
  | isLoadingChild robot && isCorral env robotPosition && (childrenAmountInPosition env robotPosition == 1) = (env, RDropChild, gen)
  | isLoadingChild robot && isDirty env robotPosition = (env, RClean, gen)
  | isLoadingChild robot =
    let direction = nextDirectionToNearbyEmptyCorral env (position robot) []
        action =
          if isJust direction
            then fromJust $ robotDirectionToAction (fromJust direction)
            else RStay
     in (env, action, gen)
  | not (isLoadingChild robot) && not (anyKidOutSideCorral env) && isDirty env robotPosition = (env, RClean, gen)
  | not (isLoadingChild robot) && not (anyKidOutSideCorral env) && anyDirtyCell env =
    let direction = nextDirectionToNearestDirtyCell env (position robot) []
        action =
          if isJust direction
            then fromJust $ robotDirectionToAction (fromJust direction)
            else RStay
     in (env, action, gen)
  | not (isLoadingChild robot) && anyKidOutSideCorral env =
    let direction = nextDirectionToBestChildToLoad env (position robot)
        action =
          if isJust direction
            then fromJust $ robotDirectionToAction (fromJust direction)
            else RStay
     in (env, action, gen)
  | otherwise = (env, RStay, gen)
  where
    robot = robots env !! index
    robotPosition = position robot
