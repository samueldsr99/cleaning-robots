-- Module for console output
module UI
  ( clearScreen,
    printEnvironment,
  )
where

import Control.Monad (when)
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import qualified Data.Maybe
import Types (Child (..), Corral (Corral), Dirt (Dirt), Environment (..), Obstacle (Obstacle), Robot (..))
import Utils (getChildrenPositions, getDirtPositions, getRobotsPositions, robotInCell)

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

showRobot :: Maybe Robot -> String
showRobot robot =
  if isNothing robot
    then ""
    else "R" ++ show (idx $ fromJust robot)

showChild :: Maybe Child -> String
showChild child =
  if isNothing child
    then ""
    else "c"

showDirt :: Maybe Dirt -> String
showDirt dirt =
  if isNothing dirt
    then ""
    else "D"

showCorral :: Maybe Corral -> String
showCorral corral =
  if isNothing corral
    then ""
    else "C"

showObstacle :: Maybe Obstacle -> String
showObstacle obstacle =
  if isNothing obstacle
    then ""
    else "O"

showValueOfCell :: Environment -> Int -> Int -> String
showValueOfCell env r c =
  let robotInCell = find (\Robot {position = pos} -> (r, c) == pos) (robots env)
      childInCell = find (\(Child x y) -> (r, c) == (x, y)) (children env)
      dirtInCell = find (\(Dirt x y) -> (r, c) == (x, y)) (dirt env)
      corralInCell = find (\(Corral x y) -> (r, c) == (x, y)) (corrals env)
      obstacleInCell = find (\(Obstacle x y) -> (r, c) == (x, y)) (obstacles env)

      cellStr =
        showRobot robotInCell
          ++ showChild childInCell
          ++ showDirt dirtInCell
          ++ showCorral corralInCell
          ++ showObstacle obstacleInCell
   in cellStr
        -- Fill with Spaces
        ++ concat (replicate (4 - length cellStr) " ")

printHorizontalDivision :: Int -> Int -> IO ()
printHorizontalDivision row length =
  let value =
        if row == 0
          then " " ++ concat (replicate length "____ ")
          else "|" ++ concat (replicate length "____|")
   in putStrLn value

printRow :: Int -> Environment -> IO ()
printRow row env =
  let _m = m env
      value = "|" ++ concat ([showValueOfCell env row i ++ "|" | i <- [0 .. _m - 1]])
   in putStrLn value

printRows :: Int -> Environment -> IO ()
printRows currentRow env =
  when (currentRow < n env) $ do
    printRow currentRow env
    printHorizontalDivision (currentRow + 1) (m env)
    printRows (currentRow + 1) env

printEnvironment :: Environment -> IO ()
printEnvironment env = do
  printHorizontalDivision 0 (m env)
  printRows 0 env

  print env
