-- Data Types
module Types
  ( Environment (..),
    Robot (..),
    RType (String),
    Child (Child),
    Dirt (Dirt),
    Corral (Corral),
    Obstacle (Obstacle),
  )
where

data RType = String deriving (Show)

data Robot = Robot
  { idx :: Int,
    rtype :: String,
    position :: (Int, Int),
    loadingChild :: Bool
  }
  deriving (Show)

data Dirt = Dirt Int Int deriving (Show)

data Child = Child Int Int deriving (Show)

data Corral = Corral Int Int deriving (Show)

data Obstacle = Obstacle Int Int deriving (Show)

data Environment = Environment
  { n :: Int,
    m :: Int,
    robotsAmount :: Int,
    childrenAmount :: Int,
    robots :: [Robot],
    children :: [Child],
    dirt :: [Dirt],
    corrals :: [Corral],
    obstacles :: [Obstacle]
  }
  deriving (Show)
