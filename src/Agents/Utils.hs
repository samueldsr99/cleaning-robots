module Agents.Utils where

import Data.List (elemIndex, findIndex, nub, sortOn)
import Data.Maybe (fromJust, isJust, isNothing)
import Debug.Trace (trace)
import Types
  ( Child (..),
    Direction (..),
    Dirt (Dirt),
    Environment (..),
    Robot (..),
    RobotAction (..),
  )
import Utils (adjacentCell, canMoveRobot, canMoveRobotInPosition, getChildInCell, getCorralInCell, getDirtInCell)

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
  any (\(Child r c) -> isNothing $ getCorralInCell env r c) (children env)

-- Check whether exists a dirty cell
anyDirtyCell :: Environment -> Bool
anyDirtyCell env = not $ null (dirt env)

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

nextDirectionToNearbyEmptyCorral :: Environment -> (Int, Int) -> Direction
nextDirectionToNearbyEmptyCorral env (r, c) =
  let availableDirections =
        [ direction
          | direction <- [DUp, DRight, DDown, DLeft],
            let adj = adjacentCell env (r, c) direction
             in isJust adj && canMoveRobotInPosition env (r, c) direction
        ]
      neighbours =
        [(fromJust $ adjacentCell env (r, c) direction, direction) | direction <- availableDirections]
      reachable_positions_from_neighbours =
        [ bfs env [BfsState (r, c) 0] [BfsState (r, c) 0]
          | ((r, c), _) <- neighbours
        ]
      filter_corral_positions = filter (\(BfsState (r, c) _) -> isFreeCorral env (r, c))
      freeCorralPositionsFromNeighbours = map filter_corral_positions reachable_positions_from_neighbours
      minFreeCorralPositionFromNeighbours =
        [ head $ sortOn (\(BfsState _ distance) -> distance) x
          | x <- freeCorralPositionsFromNeighbours
        ]
      minDistanceCorral = head $ sortOn (\(BfsState _ distance) -> distance) minFreeCorralPositionFromNeighbours
      minDirectionIndex = fromJust $ elemIndex minDistanceCorral minFreeCorralPositionFromNeighbours
   in availableDirections !! minDirectionIndex

-- Get the bfs states of all children reachable from (r, c) walking as a robot
reachableChildrenStates :: Environment -> (Int, Int) -> [BfsState]
reachableChildrenStates env (r, c) =
  let states = bfs env [BfsState (r, c) 0] [BfsState (r, c) 0]
      reachableChildrenStates =
        filter
          ( \(BfsState (r, c) _) ->
              isJust (getChildInCell env r c)
                && isNothing (getCorralInCell env r c)
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

-- Get the next direction to reach the best child to load starting at (r, c)
nextDirectionToBestChildToLoad :: Environment -> (Int, Int) -> Direction
nextDirectionToBestChildToLoad env (r, c) =
  let availableDirections =
        [ direction
          | direction <- [DUp, DRight, DDown, DLeft],
            let adj = adjacentCell env (r, c) direction
             in isJust adj && canMoveRobotInPosition env (r, c) direction
        ]
      neighbours =
        [(fromJust $ adjacentCell env (r, c) direction, direction) | direction <- availableDirections]
      reachableChildrenFromNeighbours =
        [reachableChildrenStates env pos | (pos, _) <- neighbours]
      bestChildToLoadFromNeighbours =
        [fromJust $ bestChildToLoad env state | state <- reachableChildrenFromNeighbours]
      bestChild = head $ sortOn (\(BfsState pos distance) -> distance) bestChildToLoadFromNeighbours
      bestChildIndex = fromJust $ elemIndex bestChild bestChildToLoadFromNeighbours
   in availableDirections !! bestChildIndex

-- Get the nearest dirty cell starting at (r, c)
nearestDirtyCell :: Environment -> (Int, Int) -> Maybe BfsState
nearestDirtyCell env curPos =
  let states = bfs env [BfsState curPos 0] [BfsState curPos 0]
      dirtyCells = filter (\(BfsState (r, c) _) -> isJust $ getDirtInCell env r c) states
   in if not (null dirtyCells)
        then Just $ head $ sortOn (\(BfsState pos distance) -> distance) dirtyCells
        else Nothing

-- Get the next direction in the path to reach the nearest dirty cell starting at (r, c)
nextDirectionToNearestDirtyCell :: Environment -> (Int, Int) -> Direction
nextDirectionToNearestDirtyCell env (r, c) =
  let availableDirections =
        [ direction
          | direction <- [DUp, DRight, DDown, DLeft],
            let adj = adjacentCell env (r, c) direction
             in isJust adj && canMoveRobotInPosition env (r, c) direction
        ]
      neighbours =
        [(fromJust $ adjacentCell env (r, c) direction, direction) | direction <- availableDirections]
      nearestDirtyCellFromNeighbours =
        [fromJust $ nearestDirtyCell env pos | (pos, _) <- neighbours]
      nearestCell = head $ sortOn (\(BfsState pos distance) -> distance) nearestDirtyCellFromNeighbours
      nearestCellIndex = fromJust $ elemIndex nearestCell nearestDirtyCellFromNeighbours
   in availableDirections !! nearestCellIndex
