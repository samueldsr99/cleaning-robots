-- Main module for doing simulations
module Simulation (simulate) where

import Agents.ReactiveAgent (getAction)
import Debug.Trace (trace)
import Env (applyRobotAction, genInitialState, moveChildRandomly, moveChildrenRandomly, shuffleEnv)
import System.Random (StdGen, mkStdGen)
import Types (Environment (robotsAmount), RobotAction (RClean, RUp))
import UI (clearScreen, printEnvironment)
import Utils (dirtAmount, emptyCells, freeCellsPercent, getRandomCellsInSquareNotContaining)

printStats :: Environment -> IO ()
printStats env = do
  putStrLn $ "Free cells: " ++ show (emptyCells env)
  putStrLn $ "Dirty cells: " ++ show (dirtAmount env)
  putStrLn $ "Free cells percent: " ++ show (freeCellsPercent env)

doCycle :: Environment -> Int -> Int -> StdGen -> IO ()
doCycle env epoch timeToShuffleEnv gen = do
  putStrLn $ "Epoch: " ++ show epoch
  printStats env
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
      (newEnv3, newGen3) =
        if epoch `mod` timeToShuffleEnv == 0
          then shuffleEnv (newEnv2, newGen2)
          else (newEnv2, newGen2)
   in doCycle newEnv3 (epoch + 1) timeToShuffleEnv newGen3

simulate :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
simulate n m children robots obstacles dirt timeToShuffleEnv seed =
  let gen = mkStdGen seed
      (initialState, newGen) = genInitialState n m children robots obstacles dirt gen
   in doCycle initialState 0 timeToShuffleEnv newGen
