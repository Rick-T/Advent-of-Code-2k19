module Day17 where

import Paths_Advent_of_Code_2k19
import Common.GridMovement
import Common.Intcode
import Common.Util
import Control.Monad.RWS.Strict
import Control.Monad.State
import Data.Char (chr, intToDigit, ord)
import Data.List (inits, intercalate, permutations)
import Data.List.Split
import Data.Map.Strict as M (Map(..), filter, fromList, keys, lookup, toList)

type CameraView = Map Position Char

data Robot = Robot {pos :: Position, dir :: Direction} deriving Show

type RobotState = State Robot

data Instruction = R | L | Move Int deriving Eq

type Function = [Instruction]

data FuncRef = A | B | C deriving Eq

type Routine = [FuncRef]

instance Show FuncRef where
    show A = "A"
    show B = "B"
    show C = "C"
    showList = foldl (\f p -> f . (',' :) . shows p) tail

instance Show Instruction where
    show L        = "L"
    show R        = "R"
    show (Move i) = show i
    showList = foldl (\f p -> f . (',' :) . shows p) tail

part1 :: IO Int
part1 = sum . fmap alignment . intersections . displayView <$> solve runProgram

part2 :: IO Int
part2 = do
    path <- part2a
    let (main, functions) = findRoutine path
    last <$> solve (writeAddr 0 2 *> runRobot main functions False)

part2a :: IO [Instruction]
part2a = do
    camera <- displayView <$> solve runProgram
    return $ evalState (totalPath camera) $ findStart camera

part2b :: IO Int
part2b = last <$> solve (writeAddr 0 2 *> runRobot mainRoutine (funcA, funcB, funcC) False)

solve :: ComputerState a -> IO [Val]
solve state = snd . evalRWS state 0 <$> loadComputer "input/Day17.txt"

findRoutine :: [Instruction] -> (Routine, (Function, Function, Function))
findRoutine instABC = head
    [ (routine, (a, b, c))
    | len <- [3 .. 10]
    , a   <- possibleFunctions instABC
    , let instBC = concat $ splitOn a instABC
    , b <- possibleFunctions instBC
    , let instC = concat $ splitOn b instBC
    , c <- possibleFunctions instC
    , let rest = concat $ splitOn c instC
    , null rest
    , nA <- [1 .. len]
    , nB <- [1 .. len - nA]
    , let nC = len - nA - nB
    , nA' <- [1 .. nA]
    , nB' <- [1 .. nB]
    , let
        f h
            | h == A = a
            | h == B = b
            | h == C = c
    , perm <- permutations $ replicate (nA - nA') A ++ replicate (nB - nB') B ++ replicate nC C
    , null perm || head perm /= B
    , let routine = replicate nA' A ++ replicate nB' B ++ perm
    , concatMap f routine == instABC
    ]

possibleFunctions :: [Instruction] -> [Function]
possibleFunctions []    = []
possibleFunctions funcs = Prelude.filter isValidFunction $ everySecond $ drop 2 $ inits funcs

isValidFunction :: Function -> Bool
isValidFunction f = length (compile f) <= 20

runRobot :: Routine -> (Function, Function, Function) -> Bool -> ComputerState ()
runRobot routine (fA, fB, fC) feed = do
    let feedInstruction = fmap ord [if feed then 'y' else 'n', '\n']
    feedInput $ compile routine
    feedInput $ compile fA
    feedInput $ compile fB
    feedInput $ compile fC
    feedInput $ feedInstruction
    runProgram

totalPath :: CameraView -> RobotState [Instruction]
totalPath camera = do
    (Robot pos dir) <- get
    let (turn, dir') = if M.lookup (move pos (turnLeft dir)) camera == Just '#' then (L, turnLeft dir) else (R, turnRight dir)
    let step         = length $ takeWhile (== Just '#') $ fmap (\p -> M.lookup p camera) $ tail $ iterate (flip move dir') pos
    if step == 0
        then return []
        else do
            put (Robot (moveN step pos dir') dir')
            nextSegment <- totalPath camera
            return (turn : Move step : nextSegment)

findStart :: CameraView -> Robot
findStart camera =
    let (position, direction) = head $ toList $ M.filter (\c -> c /= '#' && c /= '.') camera
    in
        Robot position $ case direction of
            '^' -> North
            'v' -> South
            '>' -> East
            '<' -> West

alignment :: Position -> Int
alignment = uncurry (*)

displayView :: [Val] -> CameraView
displayView input = fromList [ ((x, y), c) | (y, l) <- zip [0 ..] $ splitOn "\n" $ fmap (chr . fromIntegral) input, (x, c) <- zip [0 ..] l ]

intersections :: CameraView -> [Position]
intersections camera = Prelude.filter (isIntersection camera) $ keys camera

isIntersection :: CameraView -> Position -> Bool
isIntersection camera (x, y) = all (\p -> M.lookup p camera == Just '#') [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

compile :: Show a => a -> [Int]
compile s = (ord <$> show s) ++ [ord '\n']

mainRoutine :: Routine
mainRoutine = [A, B, B, A, B, C, A, C, B, C]

funcA :: Function
funcA = [L, Move 4, L, Move 6, L, Move 8, L, Move 12]

funcB :: Function
funcB = [L, Move 8, R, Move 12, L, Move 12]

funcC :: Function
funcC = [R, Move 12, L, Move 6, L, Move 6, L, Move 8]
