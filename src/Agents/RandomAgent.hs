-- Agent that makes random actions
module Agents.RandomAgent where

import Env (robotActions)
import System.Random
  ( Random (randomR),
    StdGen,
  )
import Types
  ( Environment (..),
    Robot (..),
    RobotAction (..),
  )

-- Get a random action from available actions
getAction :: (Environment, Int) -> StdGen -> (RobotAction, StdGen)
getAction (env, index) gen =
  let availableActions = robotActions env index
      (r, newGen) = randomR (0, length availableActions - 1) gen :: (Int, StdGen)
   in (availableActions !! r, newGen)
