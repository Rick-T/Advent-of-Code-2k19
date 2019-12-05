module Day03 where

import Paths_Advent_of_Code_2k19
import Common.Parsers
import Data.Foldable
import Data.List
import Data.Ord
import Text.Parsec                    ( parse )
import Text.Parsec.String             ( Parser, parseFromFile )
import Text.Parsec.Char               ( oneOf, char, digit, endOfLine, letter )
import Text.Parsec.Combinator         ( many1, sepBy1 )
import Data.Set                       ( Set, intersection, fromList, map )

data Direction = U | D | L | R deriving Show

data Instruction = Instruction Direction Int deriving Show

type WireInstructions = [Instruction]

type Location = (Int, Int)

type Wire = [Location]

solvePart1 :: IO Int
solvePart1 = fmap (manhattanDistance . closestIntersection) loadWires

solvePart2 :: IO Int
solvePart2 = fmap optimalDistance loadWires

closestIntersection :: (Wire, Wire) -> Location
closestIntersection wires = (!! 1) $ sortOn manhattanDistance $ toList $ intersections wires

manhattanDistance :: Location -> Int
manhattanDistance (x, y) = abs x + abs y

intersections :: (Wire, Wire) -> Set Location
intersections = uncurry intersection . mapBoth fromList

optimalDistance :: (Wire, Wire) -> Int
optimalDistance wires@(w, v) = (!! 1) $ sort $ toList $ Data.Set.map (\int -> distanceTo int w + distanceTo int v) $ intersections wires

distanceTo :: Location -> Wire -> Int
distanceTo loc w = length $ takeWhile (/= loc) w

wire :: WireInstructions -> Wire
wire inst = reverse $ foldl (flip step) [] inst

step :: Instruction -> Wire -> Wire
step inst              []   = step inst [(0, 0)]
step (Instruction _ 0) wire = wire
step (Instruction dir num) wire = let
    next = moveDir dir $ head wire
    in
        step (Instruction dir (num - 1)) $ next : wire

moveDir :: Direction -> Location -> Location
moveDir L (x, y) = (x - 1, y)
moveDir D (x, y) = (x, y - 1)
moveDir U (x, y) = (x, y + 1)
moveDir R (x, y) = (x + 1, y)

loadWires :: IO (Wire, Wire)
loadWires = do
    fileName <- getDataFileName "input/Day03/input01.txt"
    result   <- parseFromFile wiresInstructionsP fileName
    case result of
        Left  err  -> error $ show err
        Right succ -> return $ mapBoth wire succ

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

directionP :: Parser Direction
directionP = do
    dir <- letter
    return $ case dir of
        'U' -> U
        'D' -> D
        'R' -> R
        'L' -> L

instructionP :: Parser Instruction
instructionP = do
    dir <- directionP
    Instruction dir <$> num

wireInstructionsP :: Parser WireInstructions
wireInstructionsP = instructionP `sepBy1` char ','

wiresInstructionsP :: Parser (WireInstructions, WireInstructions)
wiresInstructionsP = do
    first  <- wireInstructionsP
    _      <- endOfLine
    second <- wireInstructionsP
    return (first, second)
