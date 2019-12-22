module Day21 where

import Paths_Advent_of_Code_2k19
import Common.GridMovement
import Common.Intcode
import Common.Util
import Control.Monad
import Control.Monad.RWS.Lazy
import Control.Monad.State
import Data.Char (chr, intToDigit, ord)
import Data.List (inits, intersperse, permutations)
import Data.List.Split
import Data.Map.Strict as M (Map(..), filter, fromList, keys, lookup, toList)

type CameraView = Map Position Char

data Robot = Robot {pos :: Position, dir :: Direction} deriving Show

type RobotState = State Robot

data Command = AND | OR | NOT deriving Show

data Register = A | B | C | D | E | F | G | H | I | T | J deriving Show

data Instruction = Instruction (Command, Register, Register) | Walk | Run

instance Show Instruction where
    show (Instruction (c, r, w)) = show c ++ " " ++ show r ++ " " ++ show w
    show Walk                    = "WALK"
    show Run                     = "RUN"

part1 :: IO [Val]
part1 = do
    computer <- loadComputer "input/Day21.txt"
    let program = Instruction <$> [(NOT, A, J), (NOT, B, T), (OR, T, J), (NOT, C, T), (AND, B, T), (OR, T, J), (AND, D, J)]
    return $ snd $ evalRWS (runRobot program Walk) 0 computer

part2 :: IO [Val]
part2 = do
    computer <- loadComputer "input/Day21.txt"
    let
        program =
            Instruction <$> [(OR, B, T), (AND, C, T), (NOT, T, J), (NOT, A, T), (OR, T, J), (AND, D, J), (NOT, H, T), (NOT, T, T), (OR, E, T), (AND, T, J)]
    return $ snd $ evalRWS (runRobot program Run) 0 computer

compile :: Show a => a -> [Int]
compile s = (ord <$> show s) ++ [ord '\n']

runRobot :: [Instruction] -> Instruction -> ComputerState ()
runRobot instructions goCommand = do
    forM_ instructions $ feedInput . compile
    feedInput $ compile goCommand
    runProgram
