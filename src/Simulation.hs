-- Main module for doing simulations
module Simulation (simulate) where

import Env (applyRobotAction, genInitialState, moveChildRandomly, moveChildrenRandomly)
import System.Random (StdGen, mkStdGen)
import Types (Environment, RobotAction (RClean, RUp))
import UI (clearScreen, printEnvironment)
import Utils (getRandomCellsInSquareNotContaining)

doCycle :: Environment -> Int -> StdGen -> IO ()
doCycle env count gen = do
  putStrLn $ "Epoch: " ++ show count
  printEnvironment env
  _ <- getLine
  let (newEnv, newGen) = moveChildrenRandomly env gen
      newEnv2 = applyRobotAction (newEnv, 0) (if count < 5 then RUp else RClean)
   in doCycle newEnv2 (count + 1) newGen

simulate :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
simulate n m children robots obstacles dirt seed =
  let gen = mkStdGen seed
      initialState = genInitialState n m children robots obstacles dirt seed
   in doCycle initialState 0 gen
