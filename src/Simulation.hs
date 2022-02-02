-- Main module for doing simulations
module Simulation (simulate) where

import Agents.ReactiveAgent (getAction)
import Debug.Trace (trace)
import Env (applyRobotAction, genInitialState, moveChildRandomly, moveChildrenRandomly)
import System.Random (StdGen, mkStdGen)
import Types (Environment (robotsAmount), RobotAction (RClean, RUp))
import UI (clearScreen, printEnvironment)
import Utils (getRandomCellsInSquareNotContaining)

doCycle :: Environment -> Int -> StdGen -> IO ()
doCycle env count gen = do
  putStrLn $ "Epoch: " ++ show count
  printEnvironment env
  _ <- getLine
  let (newEnv, newGen) = moveChildrenRandomly env gen
      processRobotAction =
        ( \(env_, gen_) index_ ->
            let (robotAction_, newGen_) = getAction (env_, index_) gen_
                newEnv_ = applyRobotAction (env_, index_) robotAction_
             in (newEnv_, newGen_)
        )
      (newEnv2, newGen2) = foldl processRobotAction (newEnv, newGen) [0 .. robotsAmount env - 1]
   in doCycle newEnv2 (count + 1) newGen2

simulate :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
simulate n m children robots obstacles dirt seed =
  let gen = mkStdGen seed
      initialState = genInitialState n m children robots obstacles dirt seed
   in doCycle initialState 0 gen
