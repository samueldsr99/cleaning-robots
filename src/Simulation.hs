-- Main module for doing simulations
module Simulation (simulate) where

import Env (genInitialState)
import Types (Environment)
import UI (clearScreen, printEnvironment)
import Utils (getRandomCellsInSquareNotContaining)

doCycle :: Environment -> Int -> IO ()
doCycle env count =
  let
   in do
        putStrLn $ "Epoch: " ++ show count
        printEnvironment env
        _ <- getLine
        -- Next Env Step

        doCycle env (count + 1)

simulate :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
simulate n m children robots obstacles dirt seed =
  let initialState = genInitialState n m children robots obstacles dirt seed
   in doCycle initialState 0
