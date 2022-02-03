-- Data Types
module Types
  ( Environment (..),
    Robot (..),
    Child (Child),
    Dirt (Dirt),
    Corral (Corral),
    Obstacle (Obstacle),
    ChildAction (..),
    RobotAction (..),
    Direction (..),
    RObjective (..),
  )
where

data RObjective = RObjective
  { otype :: String,
    opos :: (Int, Int)
  }
  deriving (Show)

data Robot = Robot
  { idx :: Int,
    rtype :: String,
    position :: (Int, Int),
    loadingChild :: Maybe Int,
    objective :: Maybe RObjective
  }
  deriving (Show)

data Dirt = Dirt Int Int deriving (Eq, Show)

data Child = Child Int Int deriving (Eq, Show)

data Corral = Corral Int Int deriving (Eq, Show)

data Obstacle = Obstacle Int Int deriving (Eq, Show)

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

data Direction
  = DUp
  | DRight
  | DLeft
  | DDown
  | DDiagonalUpLeft
  | DDiagonalUpRight
  | DDiagonalDownLeft
  | DDiagonalDownRight
  deriving (Show, Eq)

data ChildAction = CUp | CRight | CDown | CLeft | CStay deriving (Show, Eq)

data RobotAction
  = RUp
  | RRight
  | RDown
  | RLeft
  | RStay
  | RDropChild
  | RClean
  deriving (Show, Eq)
