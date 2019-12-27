module Day15 where

import Paths_Advent_of_Code_2k19
import Common.Intcode
import Common.Unicode
import Common.Util
import Control.Monad
import Control.Monad.RWS.Strict
import Data.Map.Strict as M
import System.Console.ANSI

data Direction = North | South | East | West deriving Show

data Status = Bonked | Moved | Oxygen deriving (Show, Eq)

type Position = (Val, Val)

type AreaMap = Map Position Status

type FloodMap = Map Position Int

data Robot = Robot {pos :: Position, breadcrumbs :: [Direction], area :: AreaMap} deriving Show

type DrawHandler = Robot -> IO ()

type RobotState = RWST DrawHandler () Robot ComputerStateIO

type InputMethod = RobotState Val

type OutputMethod = Status -> RobotState Bool

part1 :: IO Int
part1 = solve noOutput oxygenDistance

part2 :: IO Int
part2 = solve noOutput longestPath

animateExploration :: IO ()
animateExploration = hideCursor *> clearScreen *> solve drawInTerminal fullMap *> showCursor

solve :: DrawHandler -> RobotState a -> IO a
solve drawHandler state = fmap fst (evalRWST (fst <$> evalRWST state drawHandler initRobot) 0 =<< loadComputer "input/Day15.txt")

longestPath :: RobotState Int
longestPath = maximum . elems <$> (floodMap =<< oxygenPos)

oxygenDistance :: RobotState Int
oxygenDistance = (!) <$> floodMap (0, 0) <*> oxygenPos

floodMap :: Position -> RobotState FloodMap
floodMap position = flood 0 position . keys . M.filter (/= Bonked) <$> fullMap

oxygenPos :: RobotState Position
oxygenPos = head . keys . M.filter (== Oxygen) <$> fullMap

fullMap :: RobotState AreaMap
fullMap = explore *> gets area

flood :: Int -> Position -> [Position] -> Map Position Int
flood val start positions =
  let floodTo = Prelude.filter (`elem` positions) $ flip move start <$> directions
  in Prelude.foldr (\pos m -> union m (flood (val + 1) pos (Prelude.filter (/= pos) positions))) (singleton start val) floodTo

explore :: RobotState ()
explore = do
  (Robot position crumbs areaMap) <- get
  case viableDirections areaMap position of
    [] -> case crumbs of
      [] -> return ()
      _  -> backtrace *> explore
    (dir : _) -> step dir *> explore

viableDirections :: AreaMap -> Position -> [Direction]
viableDirections areaMap position = Prelude.filter (\dir -> move dir position `notMember` areaMap) directions

backtrace :: RobotState ()
backtrace = do
  crumbs <- gets breadcrumbs
  unless (Prelude.null crumbs) $ do
    step $ opposite $ head crumbs
    modify $ \r -> r { breadcrumbs = tail crumbs }

step :: Direction -> RobotState Status
step command = do
  status                          <- lift $ withInput (toInput command) $ toStatus . head <$> nextOutputs 1
  (Robot position crumbs areaMap) <- get
  let targetPos = move command position
  case status of
    Bonked -> modify $ \r -> r { area = insert targetPos status areaMap }
    _      -> put $ Robot { pos = targetPos, breadcrumbs = command : crumbs, area = insert targetPos status areaMap }
  liftIO =<< ask <*> get
  return status

drawInTerminal :: DrawHandler
drawInTerminal (Robot position _ areaMap) = setCursorPosition 0 0 *> (printMap id $ insert position robotFace $ M.map toReadable $ areaMap)

noOutput :: DrawHandler
noOutput = return . const ()

move :: Direction -> Position -> Position
move North (x, y) = (x, y - 1)
move South (x, y) = (x, y + 1)
move East  (x, y) = (x + 1, y)
move West  (x, y) = (x - 1, y)

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East  = West
opposite West  = East

toInput :: Direction -> Val
toInput North = 1
toInput South = 2
toInput West  = 3
toInput East  = 4

toStatus :: Val -> Status
toStatus 0 = Bonked
toStatus 1 = Moved
toStatus 2 = Oxygen

toReadable :: Status -> Char
toReadable Bonked = fullBlock
toReadable Moved  = lightShade
toReadable Oxygen = alchemicalSymbolForAir

directions :: [Direction]
directions = [North, West, East, South]

initRobot :: Robot
initRobot = Robot (0, 0) [] $ singleton (0, 0) Moved
