module Main where

import Simulation (simulate)

-- Simulation params
n :: Int
n = 6

m :: Int
m = 6

children :: Int
children = 1

robots :: Int
robots = 0

seed :: Int
seed = 98

obstacles :: Int
obstacles = 2

dirt :: Int
dirt = 0

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
  putStrLn "__________________________________________\n"

  simulate n m children robots obstacles dirt seed
