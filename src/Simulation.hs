-- Main module for doing simulations
module Simulation (simulate) where

import Agents.ReactiveAgent (getAction)
import Debug.Trace (trace)
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
      (robotAction, newGen2) = getAction (env, 0) newGen
      newEnv2 = applyRobotAction (newEnv, 0) robotAction
   in do
        putStrLn $ "robot 0 action: " ++ show robotAction
        doCycle newEnv2 (count + 1) newGen2

simulate :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
simulate n m children robots obstacles dirt seed =
  let gen = mkStdGen seed
      initialState = genInitialState n m children robots obstacles dirt seed
   in doCycle initialState 0 gen
