module Main where

import Simulation (simulate)

-- Simulation params
n :: Int
n = 12

m :: Int
m = 12

children :: Int
children = 7

robots :: Int
robots = 4

seed :: Int
seed = 10

obstacles :: Int
obstacles = 7

dirt :: Int
dirt = 8

timeToShuffleEnv :: Int
timeToShuffleEnv = 100

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
