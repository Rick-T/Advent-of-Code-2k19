module Day11 (part1, part2) where

import Paths_Advent_of_Code_2k19
import Common.Intcode
import Control.Monad.RWS.Lazy
import Control.Monad.State
import Data.List.Split (chunksOf)
import Data.Map.Strict
import Data.Ord (comparing)

type Color = Val

type Position = (Int, Int)

data Direction = U | D | L | R deriving Show

data Robot = Robot { pos :: Position, dir :: Direction, hull :: Map Position Color } deriving Show

type RobotState = StateT Robot ComputerState

part1 :: IO Int
part1 = fst3 . runRWS (evalStateT (runRobot *> countVisited) initRobot) 0 <$> loadComputer "input/Day11.txt"

part2 :: IO ()
part2 = do
  painting <- fst3 . runRWS (evalStateT (runRobot *> gets hull) initRobot) 0 <$> loadComputer "input/Day11.txt"
  let ks           = keys painting
  let (xmin, ymin) = (minimum $ fst <$> ks, minimum $ snd <$> ks)
  let (xmax, ymax) = (maximum $ fst <$> ks, maximum $ snd <$> ks)
  mapM_ putStrLn $ chunksOf (xmax - xmin + 1) [ toReadable panel | y <- [ymin .. ymax], x <- [xmin .. xmax], let panel = findWithDefault 0 (x, y) painting ]

toReadable :: Val -> Char
toReadable 0 = '\x2588'
toReadable 1 = '\x2591'

runRobot :: RobotState ()
runRobot = do
  panel   <- curPanel
  outputs <- lift $ withInput panel $ nextOutputs 2
  case outputs of
    []                     -> return ()
    (color : rotation : _) -> do
      paint color
      rotate rotation
      move
      runRobot

countVisited :: RobotState Int
countVisited = length . keys <$> gets hull

curPanel :: RobotState Color
curPanel = do
  p <- gets pos
  h <- gets hull
  return $ findWithDefault 0 p h

paint :: Color -> RobotState ()
paint c = do
  p <- gets pos
  h <- gets hull
  modify (\r -> r { hull = insert p c h })

move :: RobotState ()
move = do
  (x, y) <- gets pos
  d      <- gets dir
  let
    p' = case d of
      U -> (x, y - 1)
      D -> (x, y + 1)
      L -> (x - 1, y)
      R -> (x + 1, y)
  modify (\r -> r { pos = p' })

rotate :: Val -> RobotState ()
rotate 0 = rotateLeft
rotate 1 = rotateRight

rotateRight :: RobotState ()
rotateRight = do
  facing <- gets dir
  modify
    (\r -> r
      { dir = case facing of
        U -> R
        R -> D
        D -> L
        L -> U
      }
    )

rotateLeft :: RobotState ()
rotateLeft = do
  facing <- gets dir
  modify
    (\r -> r
      { dir = case facing of
        U -> L
        L -> D
        D -> R
        R -> U
      }
    )

initRobot :: Robot
initRobot = Robot (0, 0) U $ singleton (0, 0) 1

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
