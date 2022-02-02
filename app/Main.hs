module Main where

import Simulation (simulate)

-- Simulation params
n :: Int
n = 10

m :: Int
m = 10

children :: Int
children = 5

robots :: Int
robots = 4

seed :: Int
seed = 3

obstacles :: Int
obstacles = 5

dirt :: Int
dirt = 2

timeToShuffleEnv :: Int
timeToShuffleEnv = 50

-- Start simulation
main :: IO ()
main = do
  putStrLn "__________________________________________\n"
  putStrLn $ "N: " ++ show n
  putStrLn $ "M: " ++ show m
  putStrLn $ "Robots amount: " ++ show robots
  putStrLn $ "Children amount: " ++ show children
  putStrLn $ "Obstacles amount: " ++ show obstacles
  putStrLn $ "Dirt amount: " ++ show dirt
  putStrLn $ "Time to Randomly Shuffle Environment: " ++ show timeToShuffleEnv
  putStrLn "__________________________________________\n"

  simulate n m children robots obstacles dirt timeToShuffleEnv seed
