-- Main module for doing simulations
module Simulation (simulate) where

import qualified Agents.IntelligentAgent as IntelligentAgent
import qualified Agents.ReactiveAgent as ReactiveAgent
import qualified Agents.RandomAgent as RandomAgent
import Debug.Trace (trace)
import Env (applyRobotAction, genInitialState, moveChildRandomly, moveChildrenRandomly, shuffleEnv)
import System.Random (StdGen, mkStdGen)
import Types (Environment (..), RobotAction (RClean, RUp), Robot (..))
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
            let robot = robots env_ !! index_
                rtype_ = rtype robot
                actionFunction
                  | rtype_ == "intelligent" = IntelligentAgent.getAction
                  | rtype_ == "reactive" = ReactiveAgent.getAction
                  | otherwise = RandomAgent.getAction
                (newEnv, robotAction_, newGen_) = actionFunction (env_, index_) gen_
                newEnv_ = applyRobotAction (newEnv, index_) robotAction_
             in (newEnv_, newGen_)
        )
      (newEnv2, newGen2) = foldl processRobotAction (newEnv, newGen) [0 .. robotsAmount env - 1]
      (newEnv3, newGen3) =
        if epoch `mod` timeToShuffleEnv == 0
          then shuffleEnv (newEnv2, newGen2)
          else (newEnv2, newGen2)
   in doCycle newEnv3 (epoch + 1) timeToShuffleEnv newGen3

simulate :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> String -> Int -> IO ()
simulate n m children robots obstacles dirt timeToShuffleEnv agentsType seed =
  let gen = mkStdGen seed
      (initialState, newGen) = genInitialState n m children robots obstacles dirt agentsType gen
   in doCycle initialState 1 timeToShuffleEnv newGen
